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

function updateHrefToID(a, id) {
    if (id) {
        a.href = "#" + id;
    } else {
        a.removeAttribute("href");
    }
}

var backButton = function (init) {
    var bb;
    var stack, top;

    bb = document.createElement("a");
    bb.id = "back-button";
    bb.textContent = "返回";
    updateHrefToID(bb, init);

    stack = [init];

    top = function () {
        return stack[stack.length - 1];
    };

    return {
        "element": bb,
        "push": function (id) {
            console.log("before push:", stack);
            if (id !== top()) {
                // only push new id that is different from the top
                stack.push(id);
                updateHrefToID(bb, id);
            }
            console.log("after push:", stack);
        },
        "pop": function () {
            console.log("before pop:", stack);
            if (stack.length > 1) {
                stack.pop();
            }
            updateHrefToID(bb, top());
            console.log("after pop:", stack);
        },
        "peek": top
    };
}("outline");

window.addEventListener("load", function () {
    document.body.appendChild(backButton.element);
});

window.addEventListener("hashchange", function (e) {
    var hash = decodeURI(window.location.hash);
    if (hash === "#" + backButton.peek()) {
        backButton.pop();
    }
});

window.addEventListener("click", function (e) {
    var id;

    if (e.target.tagName !== "A")
        return;

    id = getEnclosingID(e.target);
    if (id !== "back-button") {
        backButton.push(id);
    }
});
