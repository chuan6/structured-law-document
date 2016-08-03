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

function multiCols (container) {
    var i;

    container.style.columnWidth =
        container.style.MozColumnWidth =
        container.style.WebkitColumnWidth = px(300);
    container.style.columnGap =
        container.style.MozColumnGap =
        container.style.WebkitColumnGap = 0;
}

function singleCol (container) {
    var i, w;

    container.style.columnWidth =
        container.style.MozColumnWidth =
        container.style.WebkitColumnWidth = "normal";
    container.style.columnGap =
        container.style.MozColumnGap =
        container.style.WebkitColumnGap = "normal";
}

function dynamicLayoutFn(container, es) {
    var isSingleCol;
    var h = getAvailHeightFn(32);

    return function () {
        var computed, minEntryWidth, shouldBeSingleCol;

        computed = window.getComputedStyle(es[0]);
        minEntryWidth = parseFloat(computed.minWidth) + horizontalExtra(computed);
        shouldBeSingleCol = (window.innerWidth < minEntryWidth * 2);
        if (shouldBeSingleCol !== isSingleCol) {
            if (shouldBeSingleCol) {
                container.style.height = "auto";
                singleCol(container);
            } else {
                container.style.height = px(h());
                multiCols(container);
            }
            isSingleCol = shouldBeSingleCol;
        } else if (!shouldBeSingleCol) {
            container.style.height = px(h());
        }
    };
}

window.addEventListener("load", function () {
    var container = document.getElementById("entries-container");
    var entries = document.getElementsByClassName ("entry");
    var layout = dynamicLayoutFn(container, entries);

    layout();

    window.onresize = function () {
        layout();
    };
});

function getEnclosingID(elmt) {
    return elmt.id? elmt.id : getEnclosingID(elmt.parentNode);
}

function peek(arr) {
    if (arr.length > 0)
        return arr[arr.length - 1];
}

var backButton = function () {
    var bb = document.createElement("button");
    var stack = [];
    var idstr = function (s) { return "#" + s; };

    bb.id = "back-button";
    bb.textContent = "返回";

    return {
        "element": bb,
        "into": function (src) {
            var t = bb.textContent;
            var a = document.createElement("A");

            stack.push(getEnclosingID(src));
            console.log(stack);

            a.href = idstr(peek(stack));
            a.textContent = t;
            if (t) {
                bb.textContent = "";
                bb.appendChild(a);
            } else {
                bb.replaceChild(bb.childNode, a);
            }
        },
        "back": function () {
            var x = stack.pop();
            var a = bb.children[0];

            console.log(stack);
            if (x) {
                if (a.tagName === "A") {
                    a.href = idstr(x);
                }
            }
        }
    };
}();

window.addEventListener("load", function () {
    backButton.element.addEventListener("click", function (e) {
        backButton.back();
        e.stopPropagation();
    });
    document.body.appendChild(backButton.element);
});

window.addEventListener("click", function (e) {
    if (e.target.tagName !== "A")
        return;

    backButton.into(e.target);
});
