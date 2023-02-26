package com.jiachian.composerightsheet

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.BackHandler
import androidx.activity.compose.setContent
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.ColumnScope
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import com.jiachian.composerightsheet.sheet.ModalRightSheetLayout
import com.jiachian.composerightsheet.sheet.ModalRightSheetState
import com.jiachian.composerightsheet.sheet.ModalRightSheetValue
import com.jiachian.composerightsheet.sheet.rememberModalRightSheetState
import com.jiachian.composerightsheet.ui.theme.ComposeRightSheetTheme
import kotlinx.coroutines.launch

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            ComposeRightSheetTheme {
                // A surface container using the 'background' color from the theme
                Surface(
                    modifier = Modifier.fillMaxSize(),
                    color = MaterialTheme.colors.background
                ) {
                    Main()
                }
            }
        }
    }
}

@Composable
fun Main() {
    var isRightSheetVisible by remember { mutableStateOf(false) }

    Box(
        modifier = Modifier.fillMaxSize(),
        contentAlignment = Alignment.Center
    ) {
        Button(
            onClick = { isRightSheetVisible = true }
        ) {
            Text(
                text = "SHOW"
            )
        }
    }
    if (isRightSheetVisible) {
        RightSheet(
            sheetWidth = 360.dp,
            roundCorner = 16.dp,
            swipeEnabled = false,
            onDismiss = { isRightSheetVisible = false },
        ) {
            Box(
                modifier = Modifier
                    .fillMaxSize()
                    .background(Color.Black),
                contentAlignment = Alignment.Center
            ) {
                Greeting(name = "Right Sheet")
            }
        }
    }
}

@OptIn(ExperimentalMaterialApi::class)
@Composable
private fun RightSheet(
    sheetWidth: Dp = Dp.Unspecified,
    rightSheetState: ModalRightSheetState = rememberModalRightSheetState(ModalRightSheetValue.Hidden),
    roundCorner: Dp = 8.dp,
    swipeEnabled: Boolean = true,
    onDismiss: () -> Unit,
    sheetContent: @Composable ColumnScope.() -> Unit
) {
    val coroutineScope = rememberCoroutineScope()
    BackHandler {
        coroutineScope.launch { rightSheetState.dismiss(onDismiss) }
    }
    ModalRightSheetLayout(
        sheetWidth = sheetWidth,
        sheetContent = sheetContent,
        sheetShape = RoundedCornerShape(topStart = roundCorner),
        sheetState = rightSheetState,
        swipeEnabled = swipeEnabled,
        content = {},
        onDismiss = onDismiss
    )
    LaunchedEffect(rightSheetState) {
        rightSheetState.show()
    }
}

@Composable
private fun Greeting(name: String) {
    Text(
        text = "Hello $name!",
        color = Color.White
    )
}