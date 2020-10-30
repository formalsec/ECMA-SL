const fs = require("fs");

const args = process.argv;

if (args.length === 4 && args.indexOf("-f") !== -1) {

  let heapFile = args[3];

  fs.readFile(heapFile, (err, heapData) => {
    if (err) {
      throw err;
    }

    let htmlStringBody = "";
    let heapObj = JSON.parse(heapData);
    heapObj = sortObject(heapObj);

    htmlStringBody = '<ul style="padding: 0;">';

    for (let prop in heapObj) {
      let propHtmlString = "";

      for (let innerProp in heapObj[prop]) {
        let innerPropValue = heapObj[prop][innerProp];
        let innerPropHtmlString = "";

        if (String(innerPropValue).startsWith("$loc_")) {
          innerPropHtmlString += `
          <li class="expand">
            <span class="key">${innerProp}:</span>
            <span class="loc">${innerPropValue}</span>
            <a href="#${innerPropValue}">
              <span class="click-text"></span>
            </a>
          </li>`;
        } else {
          innerPropHtmlString += `
          <li>
            <span class="key">${innerProp}:</span>
            <span class="${typeof innerPropValue}">${innerPropValue}</span>
          </li>`;
        }

        // propHtmlString += `<li><span>${innerProp}: </span>${innerPropHtmlString}</li>`
        propHtmlString += innerPropHtmlString;
      }

      if (propHtmlString.length !== 0) {
        propHtmlString = `<ul>${propHtmlString}</ul>`;
      }

      htmlStringBody += `
      <li id="${prop}">
        <span class="key">${prop}: </span>
        <span>{</span>
        ${propHtmlString}
        <span>}</span>
      </li>`;
    }
    htmlStringBody += "</ul>";

    let htmlString = `
    <!doctype html5>
    <html>
      <head>
        <meta charset="utf-8">
        <link href="styles.css" rel="stylesheet" type="text/css">
      </head>
      <body>
        ${htmlStringBody}
        <script src="script.js"></script>
      </body>
    </html>`;

    fs.writeFile("heap.html", htmlString, () => "Heap.html created");
  })
}

// Taken from:
// https://stackoverflow.com/questions/5467129/sort-javascript-object-by-key
function sortObject(unordered, sortArrays = false) {
  if (!unordered || typeof unordered !== 'object') {
    return unordered;
  }

  if (Array.isArray(unordered)) {
    const newArr = unordered.map((item) => sortObject(item, sortArrays));
    if (sortArrays) {
      newArr.sort();
    }
    return newArr;
  }

  const ordered = {};
  Object.keys(unordered)
    .sort()
    .forEach((key) => {
      ordered[key] = sortObject(unordered[key], sortArrays);
    });
  return ordered;
}
