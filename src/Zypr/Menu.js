export function openUrl(url) { 
  () => {
    console.log("opening url: ", url)
    window.open(url, "_blank")   
  }
}