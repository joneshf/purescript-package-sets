function loadData(url, id) {
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
}

loadData("https://cdn.rawgit.com/joneshf/purescript-package-sets/psc-0.11.6/packages.json", "psc-container");
loadData("https://cdn.rawgit.com/joneshf/purescript-package-sets/purerl-0.11.6/packages.json", "purerl-container");
