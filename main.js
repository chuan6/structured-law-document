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

var backButton = function () {
    var bb = document.createElement("a");

    bb.id = "back-button";
    bb.textContent = "返回";

    return {
        "element": bb,
        "update": function (id) {
            if (id) {
                bb.href = "#" + id;
            } else {
                bb.removeAttribute("href");
            }
        }
    };
}();

var jumps = function (bbutton) {
    var stack = ["outline"];

    return {
        "push": function (id) {
            stack.push(id);
            bbutton.update(id);
        },
        "pop": function () {
            if (stack.length > 1) {
                stack.pop();
            }
            bbutton.update(stack[stack.length - 1]);
        },
        "peek": function () {
            if (stack.length > 0)
                return stack[stack.length - 1];
        }
    };
}(backButton);

window.addEventListener("load", function () {
    document.body.appendChild(backButton.element);
});

window.addEventListener("hashchange", function (e) {
    var hash = window.location.hash;
    if (hash === "#" + jumps.peek()) {
        jumps.pop();
    }
});

window.addEventListener("click", function (e) {
    var id;

    if (e.target.tagName !== "A")
        return;

    id = getEnclosingID(e.target);
    if (id !== "back-button") {
        jumps.push(id);
    }
});
