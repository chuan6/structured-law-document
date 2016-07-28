function px(x) {
    return "" + x + "px";
}

function getAvailHeightFn (scrollbarHeight) {
    return function () {
        return window.innerHeight - scrollbarHeight;
    };
}

function horizontalSpan(elmt) {
    var computed = window.getComputedStyle(elmt);
    return parseFloat(computed.marginLeft)
        + elmt.offsetWidth
        + parseFloat(computed.marginRight);
}

function multiCols (container, h) {
    container.style.height = px (h ());
    container.style.columnWidth =
        container.style.MozColumnWidth =
        container.style.WebkitColumnWidth = px (300);
    container.style.columnGap =
        container.style.MozColumnGap =
        container.style.WebkitColumnGap = 0;
}

function singleCol (container) {
    container.style.height = "auto";
    container.style.columnWidth =
        container.style.MozColumnWidth =
        container.style.WebkitColumnWidth = "normal";
    container.style.columnGap =
        container.style.MozColumnGap =
        container.style.WebkitColumnGap = "normal";
}

function dynamicLayout (container, h, entry) {
    if (window.innerWidth >= horizontalSpan (entry) * 2) {
        multiCols (container, h);
    } else {
        singleCol (container);
    }
}

window.addEventListener("load", function () {
    var container = document.getElementById("entries-container");
    var entries = document.getElementsByClassName ("entry");
    var h = getAvailHeightFn(32);

    dynamicLayout (container, h, entries [0]);

    window.onresize = function () {
        dynamicLayout (container, h, entries [0]);
    };
});
