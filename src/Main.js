'use strict';

exports.loadData = function(url, id) {
  return function() {
    const request = new XMLHttpRequest();
    request.open('GET', url, true);

    request.onload = function() {
      if (request.status >= 200 && request.status < 400) {
        const data = JSON.parse(request.responseText);
        const packages = Object.keys(data).join("\n");

        const container = document.getElementById(id);
        const pre = document.createElement("pre");
        const content = document.createTextNode(packages);

        pre.appendChild(content);
        container.appendChild(pre);
      } else {
        console.error("Some kind of request error", request);
      }
    };

    request.onerror = function() {
      console.error("Big time error", request);
    };

    request.send();
  };
};
