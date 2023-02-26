package com.jiachian.composerightsheet.sheet

import androidx.compose.animation.core.AnimationSpec
import androidx.compose.animation.core.TweenSpec
import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.gestures.Orientation
import androidx.compose.foundation.gestures.detectTapGestures
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.SwipeableDefaults
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.Saver
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.Shape
import androidx.compose.ui.graphics.isSpecified
import androidx.compose.ui.input.nestedscroll.nestedScroll
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.semantics.dismiss
import androidx.compose.ui.semantics.onClick
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.IntOffset
import com.jiachian.composerightsheet.sheet.ModalRightSheetState.Companion
import com.jiachian.composerightsheet.sheet.ModalRightSheetValue.Expanded
import com.jiachian.composerightsheet.sheet.ModalRightSheetValue.Hidden
import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.launch
import kotlin.math.max
import kotlin.math.roundToInt

@ExperimentalMaterialApi
enum class ModalRightSheetValue {
    Hidden,
    Expanded
}

@ExperimentalMaterialApi
class ModalRightSheetState(
    initialValue: ModalRightSheetValue,
    animationSpec: AnimationSpec<Float> = SwipeableDefaults.AnimationSpec,
    confirmStateChange: (ModalRightSheetValue) -> Boolean = { true }
) : SwipeableState<ModalRightSheetValue>(
    initialValue = initialValue,
    animationSpec = animationSpec,
    confirmStateChange = confirmStateChange
) {

    /**
     * Whether the right sheet is visible.
     */
    val isVisible: Boolean
        get() = currentValue != Hidden

    /**
     * Show the right sheet with animation and suspend until it's shown.
     * @throws [CancellationException] if the animation is interrupted
     */
    suspend fun show() {
        val targetValue = Expanded
        animateTo(targetValue = targetValue)
    }

    /**
     * Fully expand the right sheet with animation and suspend until it if fully expanded or
     * animation has been cancelled.
     * *
     * @throws [CancellationException] if the animation is interrupted
     */
    internal suspend fun expand() = animateTo(Expanded)

    /**
     * Hide the right sheet with animation and suspend until it if fully hidden or animation has
     * been cancelled.
     *
     * @throws [CancellationException] if the animation is interrupted
     */
    suspend fun hide() = animateTo(Hidden)

    suspend fun dismiss(onDismiss: () -> Unit) {
        hide()
        onDismiss()
    }

    internal val nestedScrollConnection = this.PreUpPostDownNestedScrollConnection

    companion object {

        /**
         * The default [Saver] implementation for [ModalRightSheetState].
         */
        fun Saver(
            animationSpec: AnimationSpec<Float>,
            confirmStateChange: (ModalRightSheetValue) -> Boolean
        ): Saver<ModalRightSheetState, *> = Saver(
            save = { it.currentValue },
            restore = {
                ModalRightSheetState(
                    initialValue = it,
                    animationSpec = animationSpec,
                    confirmStateChange = confirmStateChange
                )
            }
        )
    }
}

@Composable
@ExperimentalMaterialApi
fun rememberModalRightSheetState(
    initialValue: ModalRightSheetValue,
    animationSpec: AnimationSpec<Float> = SwipeableDefaults.AnimationSpec,
    confirmStateChange: (ModalRightSheetValue) -> Boolean = { true }
): ModalRightSheetState {
    return rememberSaveable(
        saver = Companion.Saver(
            animationSpec = animationSpec,
            confirmStateChange = confirmStateChange
        )
    ) {
        ModalRightSheetState(
            initialValue = initialValue,
            animationSpec = animationSpec,
            confirmStateChange = confirmStateChange
        )
    }
}

/**
 *  support [sheetWidth], [swipeEnabled], [onDismiss]
 *
 *  TODO - dismiss the right sheet after rotating screen.
 *   Current problem is that the rememberSavable object will be saved after rotating screen.
 *   So it will get the previous rememberSavable object on the next display.
 *   (I think that the composable has been destroyed, so it should get the new rememberSavable object on the next display.)
 */
@Composable
@ExperimentalMaterialApi
fun ModalRightSheetLayout(
    sheetContent: @Composable ColumnScope.() -> Unit,
    onDismiss: () -> Unit,
    modifier: Modifier = Modifier,
    sheetState: ModalRightSheetState = rememberModalRightSheetState(Hidden),
    sheetWidth: Dp = Dp.Unspecified,
    sheetShape: Shape = MaterialTheme.shapes.large,
    sheetElevation: Dp = ModalBottomSheetDefaults.Elevation,
    sheetBackgroundColor: Color = MaterialTheme.colors.surface,
    sheetContentColor: Color = contentColorFor(sheetBackgroundColor),
    scrimColor: Color = ModalBottomSheetDefaults.scrimColor,
    swipeEnabled: Boolean = true,
    content: @Composable () -> Unit
) {
    val scope = rememberCoroutineScope()
    BoxWithConstraints(modifier) {
        val fullWidth = constraints.maxWidth.toFloat()
        val sheetWidthState = remember { mutableStateOf<Float?>(null) }
        Box(Modifier.fillMaxSize()) {
            content()
            Scrim(
                color = scrimColor,
                onDismiss = { scope.launch { sheetState.dismiss(onDismiss) } },
                visible = sheetState.targetValue != Hidden
            )
        }
        Surface(
            Modifier
                .width(sheetWidth)
                .fillMaxHeight()
                .apply {
                    if (swipeEnabled) {
                        then(nestedScroll(sheetState.nestedScrollConnection))
                    }
                }
                .offset {
                    val x = if (sheetState.anchors.isEmpty()) {
                        fullWidth.roundToInt()
                    } else {
                        sheetState.offset.value.roundToInt()
                    }
                    IntOffset(x, 0)
                }
                .rightSheetSwipeable(swipeEnabled, sheetState, fullWidth, sheetWidthState)
                .onGloballyPositioned { sheetWidthState.value = it.size.width.toFloat() }
                .semantics {
                    if (sheetState.isVisible) {
                        dismiss {
                            if (sheetState.confirmStateChange(Hidden)) {
                                scope.launch { sheetState.dismiss(onDismiss) }
                            }
                            true
                        }
                    }
                },
            shape = sheetShape,
            elevation = sheetElevation,
            color = sheetBackgroundColor,
            contentColor = sheetContentColor
        ) {
            Column(content = sheetContent)
        }
        if (sheetState.currentValue != Hidden) {
            DisposableEffect(Unit) {
                // TODO - dismiss the right sheet after rotating screen.
                onDispose {
                    scope.launch {
                        sheetState.dismiss(onDismiss)
                    }
                }
            }
        }
    }
}

@Suppress("ModifierInspectorInfo")
@OptIn(ExperimentalMaterialApi::class)
private fun Modifier.rightSheetSwipeable(
    enabled: Boolean = true,
    sheetState: ModalRightSheetState,
    fullWidth: Float,
    sheetWidthState: State<Float?>
): Modifier {
    val sheetWidth = sheetWidthState.value
    val modifier = if (sheetWidth != null) {
        val anchors = if (sheetWidth < fullWidth / 2) {
            mapOf(
                fullWidth to Hidden,
                fullWidth - sheetWidth to Expanded
            )
        } else {
            mapOf(
                fullWidth to Hidden,
                max(0f, fullWidth - sheetWidth) to Expanded
            )
        }
        Modifier.swipeable(
            state = sheetState,
            anchors = anchors,
            orientation = Orientation.Horizontal,
            enabled = enabled,
            resistance = null
        )
    } else {
        Modifier
    }

    return this.then(modifier)
}

@Composable
private fun Scrim(
    color: Color,
    onDismiss: () -> Unit,
    visible: Boolean
) {
    if (color.isSpecified) {
        val alpha by animateFloatAsState(
            targetValue = if (visible) 1f else 0f,
            animationSpec = TweenSpec()
        )
        val dismissModifier = if (visible) {
            Modifier
                .pointerInput(onDismiss) { detectTapGestures { onDismiss() } }
                .semantics(mergeDescendants = true) {
                    onClick { onDismiss(); true }
                }
        } else {
            Modifier
        }

        Canvas(
            Modifier
                .fillMaxSize()
                .then(dismissModifier)
        ) {
            drawRect(color = color, alpha = alpha)
        }
    }
}