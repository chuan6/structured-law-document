function px(x) {
    return "" + x + "px";
}

function getAvailHeightFn (scrollbarHeight) {
    return function () {
        return window.innerHeight - scrollbarHeight;
    };
}

function horizontalExtra(computed) {
    var x =  parseFloat(computed.marginLeft)
        + parseFloat(computed.marginRight)
        + parseFloat(computed.borderLeftWidth)
        + parseFloat(computed.borderRightWidth)
        + parseFloat(computed.paddingLeft)
        + parseFloat(computed.paddingRight);
    return x;
}

function multiCols (container, h, es, ws) {
    var i;

    container.style.height = px (h ());
    container.style.columnWidth =
        container.style.MozColumnWidth =
        container.style.WebkitColumnWidth = px (300);
    container.style.columnGap =
        container.style.MozColumnGap =
        container.style.WebkitColumnGap = 0;

    //revert width of each entry back to min-width
    //defined in CSS
    for (i = 0; i < es.length; i++) {
        es[i].style.width = ws;
    }
}

function singleCol (container, es, computed) {
    var i, w;

    container.style.height = "auto";
    container.style.columnWidth =
        container.style.MozColumnWidth =
        container.style.WebkitColumnWidth = "normal";
    container.style.columnGap =
        container.style.MozColumnGap =
        container.style.WebkitColumnGap = "normal";

    //widden width of each entry to better utilize
    //available width
    w = window.innerWidth - horizontalExtra(computed) - 32;
    for (i = 0; i < es.length; i++) {
        es[i].style.width = px(w);
    }
}

function dynamicLayout(container, h, es) {
    var computed = window.getComputedStyle(es[0]);
    var minEntryWidth =
        parseFloat(computed.minWidth) + horizontalExtra(computed);

    if (window.innerWidth >= minEntryWidth * 2) {
        multiCols (container, h, es, computed.minWidth);
    } else {
        singleCol (container, es, computed);
    }
}

window.addEventListener("load", function () {
    var container = document.getElementById("entries-container");
    var entries = document.getElementsByClassName ("entry");
    var h = getAvailHeightFn(32);

    dynamicLayout(container, h, entries);

    window.onresize = function () {
        dynamicLayout(container, h, entries);
    };
});
