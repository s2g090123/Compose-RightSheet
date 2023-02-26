package com.jiachian.composerightsheet.sheet

import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.AnimationSpec
import androidx.compose.animation.core.SpringSpec
import androidx.compose.foundation.gestures.DraggableState
import androidx.compose.foundation.gestures.Orientation
import androidx.compose.foundation.gestures.draggable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.material.ExperimentalMaterialApi
import androidx.compose.material.ResistanceConfig
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.Saver
import androidx.compose.ui.Modifier
import androidx.compose.ui.composed
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.input.nestedscroll.NestedScrollConnection
import androidx.compose.ui.input.nestedscroll.NestedScrollSource
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.debugInspectorInfo
import androidx.compose.ui.unit.Density
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.Velocity
import androidx.compose.ui.unit.dp
import com.jiachian.composerightsheet.sheet.SwipeableDefaults.AnimationSpec
import com.jiachian.composerightsheet.sheet.SwipeableDefaults.VelocityThreshold
import com.jiachian.composerightsheet.sheet.SwipeableDefaults.resistanceConfig
import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.collect
import kotlinx.coroutines.flow.filter
import kotlinx.coroutines.flow.take
import kotlinx.coroutines.launch
import kotlin.math.abs
import kotlin.math.sign

@Stable
@ExperimentalMaterialApi
open class SwipeableState<T>(
    initialValue: T,
    internal val animationSpec: AnimationSpec<Float> = AnimationSpec,
    internal val confirmStateChange: (newValue: T) -> Boolean = { true }
) {
    var currentValue: T by mutableStateOf(initialValue)
        private set

    var isAnimationRunning: Boolean by mutableStateOf(false)
        private set

    val offset: State<Float> get() = offsetState

    val overflow: State<Float> get() = overflowState

    private val offsetState = mutableStateOf(0f)
    private val overflowState = mutableStateOf(0f)

    private val absoluteOffset = mutableStateOf(0f)

    private val animationTarget = mutableStateOf<Float?>(null)

    internal var anchors by mutableStateOf(emptyMap<Float, T>())

    private val latestNonEmptyAnchorsFlow: Flow<Map<Float, T>> =
        snapshotFlow { anchors }
            .filter { it.isNotEmpty() }
            .take(1)

    internal var minBound = Float.NEGATIVE_INFINITY
    internal var maxBound = Float.POSITIVE_INFINITY

    internal fun ensureInit(newAnchors: Map<Float, T>) {
        if (anchors.isEmpty()) {
            // need to do initial synchronization synchronously :(
            val initialOffset = newAnchors.getOffset(currentValue)
            requireNotNull(initialOffset) {
                "The initial value must have an associated anchor."
            }
            offsetState.value = initialOffset
            absoluteOffset.value = initialOffset
        }
    }

    internal suspend fun processNewAnchors(
        oldAnchors: Map<Float, T>,
        newAnchors: Map<Float, T>
    ) {
        if (oldAnchors.isEmpty()) {
            minBound = newAnchors.keys.minOrNull()!!
            maxBound = newAnchors.keys.maxOrNull()!!
            val initialOffset = newAnchors.getOffset(currentValue)
            requireNotNull(initialOffset) {
                "The initial value must have an associated anchor."
            }
            snapInternalToOffset(initialOffset)
        } else if (newAnchors != oldAnchors) {
            minBound = Float.NEGATIVE_INFINITY
            maxBound = Float.POSITIVE_INFINITY
            val animationTargetValue = animationTarget.value
            val targetOffset = if (animationTargetValue != null) {
                val oldState = oldAnchors[animationTargetValue]
                val newState = newAnchors.getOffset(oldState)
                newState ?: newAnchors.keys.minByOrNull { abs(it - animationTargetValue) }!!
            } else {
                val actualOldValue = oldAnchors[offset.value]
                val value = if (actualOldValue == currentValue) currentValue else actualOldValue
                newAnchors.getOffset(value) ?: newAnchors
                    .keys.minByOrNull { abs(it - offset.value) }!!
            }
            try {
                animateInternalToOffset(targetOffset, animationSpec)
            } catch (c: CancellationException) {
                snapInternalToOffset(targetOffset)
            } finally {
                currentValue = newAnchors.getValue(targetOffset)
                minBound = newAnchors.keys.minOrNull()!!
                maxBound = newAnchors.keys.maxOrNull()!!
            }
        }
    }

    internal var thresholds: (Float, Float) -> Float by mutableStateOf({ _, _ -> 0f })

    internal var velocityThreshold by mutableStateOf(0f)

    internal var resistance: ResistanceConfig? by mutableStateOf(null)

    internal val draggableState = DraggableState {
        val newAbsolute = absoluteOffset.value + it
        val clamped = newAbsolute.coerceIn(minBound, maxBound)
        val overflow = newAbsolute - clamped
        val resistanceDelta = resistance?.computeResistance(overflow) ?: 0f
        offsetState.value = clamped + resistanceDelta
        overflowState.value = overflow
        absoluteOffset.value = newAbsolute
    }

    private suspend fun snapInternalToOffset(target: Float) {
        draggableState.drag {
            dragBy(target - absoluteOffset.value)
        }
    }

    private suspend fun animateInternalToOffset(target: Float, spec: AnimationSpec<Float>) {
        draggableState.drag {
            var prevValue = absoluteOffset.value
            animationTarget.value = target
            isAnimationRunning = true
            try {
                Animatable(prevValue).animateTo(target, spec) {
                    dragBy(this.value - prevValue)
                    prevValue = this.value
                }
            } finally {
                animationTarget.value = null
                isAnimationRunning = false
            }
        }
    }

    @ExperimentalMaterialApi
    val targetValue: T
        get() {
            val target = animationTarget.value ?: computeTarget(
                offset = offset.value,
                lastValue = anchors.getOffset(currentValue) ?: offset.value,
                anchors = anchors.keys,
                thresholds = thresholds,
                velocity = 0f,
                velocityThreshold = Float.POSITIVE_INFINITY
            )
            return anchors[target] ?: currentValue
        }

    @ExperimentalMaterialApi
    val progress: SwipeProgress<T>
        get() {
            val bounds = findBounds(offset.value, anchors.keys)
            val from: T
            val to: T
            val fraction: Float
            when (bounds.size) {
                0 -> {
                    from = currentValue
                    to = currentValue
                    fraction = 1f
                }
                1 -> {
                    from = anchors.getValue(bounds[0])
                    to = anchors.getValue(bounds[0])
                    fraction = 1f
                }
                else -> {
                    val (a, b) =
                        if (direction > 0f) {
                            bounds[0] to bounds[1]
                        } else {
                            bounds[1] to bounds[0]
                        }
                    from = anchors.getValue(a)
                    to = anchors.getValue(b)
                    fraction = (offset.value - a) / (b - a)
                }
            }
            return SwipeProgress(from, to, fraction)
        }

    @ExperimentalMaterialApi
    val direction: Float
        get() = anchors.getOffset(currentValue)?.let { sign(offset.value - it) } ?: 0f

    @ExperimentalMaterialApi
    suspend fun snapTo(targetValue: T) {
        latestNonEmptyAnchorsFlow.collect { anchors ->
            val targetOffset = anchors.getOffset(targetValue)
            requireNotNull(targetOffset) {
                "The target value must have an associated anchor."
            }
            snapInternalToOffset(targetOffset)
            currentValue = targetValue
        }
    }

    @ExperimentalMaterialApi
    suspend fun animateTo(targetValue: T, anim: AnimationSpec<Float> = animationSpec) {
        latestNonEmptyAnchorsFlow.collect { anchors ->
            try {
                val targetOffset = anchors.getOffset(targetValue)
                requireNotNull(targetOffset) {
                    "The target value must have an associated anchor."
                }
                animateInternalToOffset(targetOffset, anim)
            } finally {
                val endOffset = absoluteOffset.value
                val endValue = anchors
                    // fighting rounding error once again, anchor should be as close as 0.5 pixels
                    .filterKeys { anchorOffset -> abs(anchorOffset - endOffset) < 0.5f }
                    .values.firstOrNull() ?: currentValue
                currentValue = endValue
            }
        }
    }

    suspend fun performFling(velocity: Float) {
        latestNonEmptyAnchorsFlow.collect { anchors ->
            val lastAnchor = anchors.getOffset(currentValue)!!
            val targetValue = computeTarget(
                offset = offset.value,
                lastValue = lastAnchor,
                anchors = anchors.keys,
                thresholds = thresholds,
                velocity = velocity,
                velocityThreshold = velocityThreshold
            )
            val targetState = anchors[targetValue]
            if (targetState != null && confirmStateChange(targetState)) animateTo(targetState)
            else animateInternalToOffset(lastAnchor, animationSpec)
        }
    }

    fun performDrag(delta: Float): Float {
        val potentiallyConsumed = absoluteOffset.value + delta
        val clamped = potentiallyConsumed.coerceIn(minBound, maxBound)
        val deltaToConsume = clamped - absoluteOffset.value
        if (abs(deltaToConsume) > 0) {
            draggableState.dispatchRawDelta(deltaToConsume)
        }
        return deltaToConsume
    }

    companion object {
        fun <T : Any> Saver(
            animationSpec: AnimationSpec<Float>,
            confirmStateChange: (T) -> Boolean
        ) = Saver<SwipeableState<T>, T>(
            save = { it.currentValue },
            restore = { SwipeableState(it, animationSpec, confirmStateChange) }
        )
    }
}

@Immutable
@ExperimentalMaterialApi
class SwipeProgress<T>(
    val from: T,
    val to: T,
    /*@FloatRange(from = 0.0, to = 1.0)*/
    val fraction: Float
) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is SwipeProgress<*>) return false

        if (from != other.from) return false
        if (to != other.to) return false
        if (fraction != other.fraction) return false

        return true
    }

    override fun hashCode(): Int {
        var result = from?.hashCode() ?: 0
        result = 31 * result + (to?.hashCode() ?: 0)
        result = 31 * result + fraction.hashCode()
        return result
    }

    override fun toString(): String {
        return "SwipeProgress(from=$from, to=$to, fraction=$fraction)"
    }
}

/**
 * Enable swipe gestures between a set of predefined states.
 *
 * To use this, you must provide a map of anchors (in pixels) to states (of type [T]).
 * Note that this map cannot be empty and cannot have two anchors mapped to the same state.
 *
 * When a swipe is detected, the offset of the [SwipeableState] will be updated with the swipe
 * delta. You should use this offset to move your content accordingly (see `Modifier.offsetPx`).
 * When the swipe ends, the offset will be animated to one of the anchors and when that anchor is
 * reached, the value of the [SwipeableState] will also be updated to the state corresponding to
 * the new anchor. The target anchor is calculated based on the provided positional [thresholds].
 *
 * Swiping is constrained between the minimum and maximum anchors. If the user attempts to swipe
 * past these bounds, a resistance effect will be applied by default. The amount of resistance at
 * each edge is specified by the [resistance] config. To disable all resistance, set it to `null`.
 *
 * For an example of a [swipeable] with three states, see:
 *
 * @sample androidx.compose.material.samples.SwipeableSample
 *
 * @param T The type of the state.
 * @param state The state of the [swipeable].
 * @param anchors Pairs of anchors and states, used to map anchors to states and vice versa.
 * @param thresholds Specifies where the thresholds between the states are. The thresholds will be
 * used to determine which state to animate to when swiping stops. This is represented as a lambda
 * that takes two states and returns the threshold between them in the form of a [ThresholdConfig].
 * Note that the order of the states corresponds to the swipe direction.
 * @param orientation The orientation in which the [swipeable] can be swiped.
 * @param enabled Whether this [swipeable] is enabled and should react to the user's input.
 * @param reverseDirection Whether to reverse the direction of the swipe, so a top to bottom
 * swipe will behave like bottom to top, and a left to right swipe will behave like right to left.
 * @param interactionSource Optional [MutableInteractionSource] that will passed on to
 * the internal [Modifier.draggable].
 * @param resistance Controls how much resistance will be applied when swiping past the bounds.
 * @param velocityThreshold The threshold (in dp per second) that the end velocity has to exceed
 * in order to animate to the next state, even if the positional [thresholds] have not been reached.
 */
@ExperimentalMaterialApi
fun <T> Modifier.swipeable(
    state: SwipeableState<T>,
    anchors: Map<Float, T>,
    orientation: Orientation,
    enabled: Boolean = true,
    reverseDirection: Boolean = false,
    interactionSource: MutableInteractionSource? = null,
    thresholds: (from: T, to: T) -> ThresholdConfig = { _, _ -> FixedThreshold(56.dp) },
    resistance: ResistanceConfig? = resistanceConfig(anchors.keys),
    velocityThreshold: Dp = VelocityThreshold
) = composed(
    inspectorInfo = debugInspectorInfo {
        name = "swipeable"
        properties["state"] = state
        properties["anchors"] = anchors
        properties["orientation"] = orientation
        properties["enabled"] = enabled
        properties["reverseDirection"] = reverseDirection
        properties["interactionSource"] = interactionSource
        properties["thresholds"] = thresholds
        properties["resistance"] = resistance
        properties["velocityThreshold"] = velocityThreshold
    }
) {
    require(anchors.isNotEmpty()) {
        "You must have at least one anchor."
    }
    require(anchors.values.distinct().count() == anchors.size) {
        "You cannot have two anchors mapped to the same state."
    }
    val density = LocalDensity.current
    state.ensureInit(anchors)
    LaunchedEffect(anchors, state) {
        val oldAnchors = state.anchors
        state.anchors = anchors
        state.resistance = resistance
        state.thresholds = { a, b ->
            val from = anchors.getValue(a)
            val to = anchors.getValue(b)
            with(thresholds(from, to)) { density.computeThreshold(a, b) }
        }
        with(density) {
            state.velocityThreshold = velocityThreshold.toPx()
        }
        state.processNewAnchors(oldAnchors, anchors)
    }

    Modifier.draggable(
        orientation = orientation,
        enabled = enabled,
        reverseDirection = reverseDirection,
        interactionSource = interactionSource,
        startDragImmediately = state.isAnimationRunning,
        onDragStopped = { velocity -> launch { state.performFling(velocity) } },
        state = state.draggableState
    )
}

@Stable
@ExperimentalMaterialApi
interface ThresholdConfig {
    fun Density.computeThreshold(fromValue: Float, toValue: Float): Float
}

@Immutable
@ExperimentalMaterialApi
data class FixedThreshold(private val offset: Dp) : ThresholdConfig {
    override fun Density.computeThreshold(fromValue: Float, toValue: Float): Float {
        return fromValue + offset.toPx() * sign(toValue - fromValue)
    }
}

@Immutable
@ExperimentalMaterialApi
data class FractionalThreshold(
    /*@FloatRange(from = 0.0, to = 1.0)*/
    private val fraction: Float
) : ThresholdConfig {
    override fun Density.computeThreshold(fromValue: Float, toValue: Float): Float {
        return (1 - fraction) * fromValue + fraction * toValue
    }
}

private fun findBounds(
    offset: Float,
    anchors: Set<Float>
): List<Float> {
    // Find the anchors the target lies between with a little bit of rounding error.
    val a = anchors.filter { it <= offset + 0.001 }.maxOrNull()
    val b = anchors.filter { it >= offset - 0.001 }.minOrNull()

    return when {
        a == null ->
            // case 1 or 3
            listOfNotNull(b)
        b == null ->
            // case 4
            listOf(a)
        a == b ->
            // case 2
            // Can't return offset itself here since it might not be exactly equal
            // to the anchor, despite being considered an exact match.
            listOf(a)
        else ->
            // case 5
            listOf(a, b)
    }
}

private fun computeTarget(
    offset: Float,
    lastValue: Float,
    anchors: Set<Float>,
    thresholds: (Float, Float) -> Float,
    velocity: Float,
    velocityThreshold: Float
): Float {
    val bounds = findBounds(offset, anchors)
    return when (bounds.size) {
        0 -> lastValue
        1 -> bounds[0]
        else -> {
            val lower = bounds[0]
            val upper = bounds[1]
            if (lastValue <= offset) {
                // Swiping from lower to upper (positive).
                if (velocity >= velocityThreshold) {
                    return upper
                } else {
                    val threshold = thresholds(lower, upper)
                    if (offset < threshold) lower else upper
                }
            } else {
                // Swiping from upper to lower (negative).
                if (velocity <= -velocityThreshold) {
                    return lower
                } else {
                    val threshold = thresholds(upper, lower)
                    if (offset > threshold) upper else lower
                }
            }
        }
    }
}

private fun <T> Map<Float, T>.getOffset(state: T): Float? {
    return entries.firstOrNull { it.value == state }?.key
}

/**
 * Contains useful defaults for [swipeable] and [SwipeableState].
 */
object SwipeableDefaults {
    /**
     * The default animation used by [SwipeableState].
     */
    val AnimationSpec = SpringSpec<Float>()

    /**
     * The default velocity threshold (1.8 dp per millisecond) used by [swipeable].
     */
    val VelocityThreshold = 125.dp

    /**
     * A stiff resistance factor which indicates that swiping isn't available right now.
     */
    const val StiffResistanceFactor = 20f

    /**
     * A standard resistance factor which indicates that the user has run out of things to see.
     */
    const val StandardResistanceFactor = 10f

    /**
     * The default resistance config used by [swipeable].
     *
     * This returns `null` if there is one anchor. If there are at least two anchors, it returns
     * a [ResistanceConfig] with the resistance basis equal to the distance between the two bounds.
     */
    fun resistanceConfig(
        anchors: Set<Float>,
        factorAtMin: Float = StandardResistanceFactor,
        factorAtMax: Float = StandardResistanceFactor
    ): ResistanceConfig? {
        return if (anchors.size <= 1) {
            null
        } else {
            val basis = anchors.maxOrNull()!! - anchors.minOrNull()!!
            ResistanceConfig(basis, factorAtMin, factorAtMax)
        }
    }
}

@ExperimentalMaterialApi
internal val <T> SwipeableState<T>.PreUpPostDownNestedScrollConnection: NestedScrollConnection
    get() = object : NestedScrollConnection {
        override fun onPreScroll(available: Offset, source: NestedScrollSource): Offset {
            val delta = available.toFloat()
            return if (delta < 0 && source == NestedScrollSource.Drag) {
                performDrag(delta).toOffset()
            } else {
                Offset.Zero
            }
        }

        override fun onPostScroll(
            consumed: Offset,
            available: Offset,
            source: NestedScrollSource
        ): Offset {
            return if (source == NestedScrollSource.Drag) {
                performDrag(available.toFloat()).toOffset()
            } else {
                Offset.Zero
            }
        }

        override suspend fun onPreFling(available: Velocity): Velocity {
            val toFling = Offset(available.x, available.y).toFloat()
            return if (toFling < 0 && offset.value > minBound) {
                performFling(velocity = toFling)
                // since we go to the anchor with tween settling, consume all for the best UX
                available
            } else {
                Velocity.Zero
            }
        }

        override suspend fun onPostFling(consumed: Velocity, available: Velocity): Velocity {
            performFling(velocity = Offset(available.x, available.y).toFloat())
            return available
        }

        private fun Float.toOffset(): Offset = Offset(0f, this)

        private fun Offset.toFloat(): Float = this.y
    }