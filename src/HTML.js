export const navigator_clipboard_writeText = (str) => () => {
  navigator.clipboard.writeText(str);
}