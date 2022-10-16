export const navigator_clipboard_writeText = (str) => () => {
  navigator.clipboard.writeText(str);
}


export const navigator_clipboard_readText = (callback) => () => {
  return navigator.clipboard.readText().then((text) =>
    callback(text)());
}