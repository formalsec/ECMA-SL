;(() => {
  function addEventListener(element) {
    let links = element.getElementsByTagName("a")
    for (let link of links) {
      link.addEventListener("click", clickEventHandler)
    }
  }

  function clickEventHandler(event) {
    event.preventDefault()

    let elem = event.currentTarget

    if (elem.hash.length === 0) {
      throw Error(
        "Click event handler applied to an element that has no hash in the href."
      )
    }

    let elemId = elem.hash.slice(1)

    let locElem = document.getElementById(elemId)

    let parentElem = elem.parentElement

    if (parentElem.classList.contains("collapse")) {
      collapseLoc(parentElem, elemId)
    } else if (parentElem.classList.contains("expand")) {
      expandLoc(parentElem, locElem.children)
    }
  }

  addEventListener(document)

  function expandLoc(elem, locChildren) {
    // Remove loc
    let elemToRemove = elem.querySelector(".loc")
    if (elemToRemove !== null) {
      elem.removeChild(elemToRemove)
    }

    // Add loc element's children
    Array.prototype.forEach.call(locChildren, (child, idx) => {
      // First child is the name of the property, don't need it.
      if (idx > 0) {
        let elemToAppend = child.cloneNode(true)

        addEventListener(elemToAppend)

        elem.append(elemToAppend)
      }
    })

    // Remove and append link to make it the last child of this parent element
    let link = elem.querySelector("a:first-of-type")
    elem.removeChild(link)
    elem.append(link)

    // Update class
    elem.classList.replace("expand", "collapse")
  }

  function collapseLoc(elem, locText) {
    let children = elem.children

    // Remove previously appended elements
    // First child is the name of the property, don't want to remove it
    // Last child is the link to expand/collapse.
    while (children.length > 2) {
      elem.removeChild(children[1])
    }

    // Append previously removed element
    let locElem = document.createElement("span")
    locElem.classList.add("loc")
    locElem.append(locText)
    elem.append(locElem)

    // Remove and append link to make it the last child of this parent element
    let link = elem.querySelector("a:first-of-type")
    elem.removeChild(link)
    elem.append(link)

    // Update class
    elem.classList.replace("collapse", "expand")
  }
})()
