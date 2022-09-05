module interactive
open Browser.Types
open Browser

// create an instance
let xhr = XMLHttpRequest.Create()  
// open the connection
xhr.``open``(method="GET", url="https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty")  
// setup the event handler that triggers when the content is loaded
xhr.onreadystatechange <- fun _ ->
    if xhr.readyState = ReadyState.Done
    then
      printfn "Status text: %s" xhr.statusText
      printfn "Status code: %d" xhr.status
      printfn "Content:\n%s" xhr.responseText

// send the request  
xhr.send()