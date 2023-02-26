# Compose-RightSheet

## About
Currently, Google only provides BottomSheet, so I have written RightSheet based on the content and structure of BottomSheet to make its usage similar.

## How to use
```kotlin

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
```

## Video
https://user-images.githubusercontent.com/32809761/221402213-ca4b7037-4f1f-4359-b0d0-c26110747f1a.mp4
