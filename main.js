function strSlice(s, end) {
    var e = escape(s), t = "";
    var i, n;

    for (i = 0; i < end && e; i++) {
        if (e[0] === "%" && e[1] === "u") {//four-digit format
            n = 6;
        } else if (e[0] === "%") {//two-digit format
            n = 3;
        } else {//one-digit format
            n = 1;
        }
        t += e.slice(0, n);
        e = e.slice(n);
    }
    return [unescape(t), e];
}

function px(x) {
    return "" + x + "px";
}

function horizontalExtra(computed, withMargin) {
    var f = (withMargin===undefined? true : withMargin);
    var x =  parseFloat(f? computed.marginLeft : "0")
        + parseFloat(f? computed.marginRight : "0")
        + parseFloat(computed.borderLeftWidth)
        + parseFloat(computed.borderRightWidth)
        + parseFloat(computed.paddingLeft)
        + parseFloat(computed.paddingRight);
    return x;
}

function getEnclosingID(elmt) {
    return elmt.id || (elmt.parentNode? getEnclosingID(elmt.parentNode) : null);
}

function updateHrefToID(a, id) {
    if (id) {
        a.href = "#" + id;
    } else {
        a.removeAttribute("href");
    }
}

function backButtonClosure(elmt, init) {
    var stack, top;

    updateHrefToID(elmt, init);

    stack = [init];

    top = function () {
        return stack[stack.length - 1];
    };

    return {
        "element": elmt,
        "push": function (id) {
            console.log("before push:", stack);
            if (id !== top()) {
                // only push new id that is different from the top
                stack.push(id);
                updateHrefToID(elmt, id);
            }
            console.log("after push:", stack);
        },
        "pop": function () {
            console.log("before pop:", stack);
            if (stack.length > 1) {
                stack.pop();
            }
            updateHrefToID(elmt, top());
            console.log("after pop:", stack);
        },
        "peek": top
    };
}

function shareButtonClosure(elmt) {
    var text, link;

    elmt.style.display = "none";

    return {
        "element": elmt,
        "showAt": function (y) {
            var top = y + window.pageYOffset - 26;

            elmt.style.top = px(top);
            elmt.style.display = "";
        },
        "setContent": function (s, ref) {
            text = s;
            link = ref;
        },
        "getContent": function () {
            var nchars = 50;
            var sliced = strSlice(text, nchars);
            return sliced[0] + (sliced[1]? "……":"") + " " + link;
        }
    };
}

function overlayClosure(elmt, content, docancel, docopy) {
    var computed, textareaWidth;

    elmt.onclick = function (e) {
        e.stopPropagation();
    };

    docancel.onclick = function (e) {
        elmt.style.display = "none";
        e.stopPropagation();
    };

    docopy.onclick = function (e) {
        content.focus();
        content.select();
        // copy might not work on some browsers, but at least
        // select() makes it easier for user to manually copy
        // the content in textarea
        document.execCommand("copy");
        // set readonly to prevent software keyboard from showing
        // on devices such as phones
        content.setAttribute("readonly", true);
        e.stopPropagation();
    };

    return {
        "element": elmt,
        "setContent": function (s) {
            content.value = s;
        },
        "show": function () {
            elmt.style.display = "block";
            computed = window.getComputedStyle(content);
            textareaWidth =
                parseFloat(window.getComputedStyle(content.parentNode).width)
                - horizontalExtra(computed, false);
            content.style.width = px(textareaWidth);
            content.style.height = px(60000 / textareaWidth);
        }
    };
}

var backButton, shareButton, overlay;

window.addEventListener("load", function () {
    backButton = backButtonClosure(
        document.getElementById("back-button"),
        "outline");

    shareButton = shareButtonClosure(
        document.getElementById("share-button"));

    overlay = overlayClosure(
        document.getElementById("overlay"),
        document.getElementById("share-text"),
        document.getElementById("cancel-overlay"),
        document.getElementById("do-copy"));
});

window.addEventListener("hashchange", function (e) {
    var hash = decodeURI(window.location.hash);

    if (hash === "#" + backButton.peek()) {
        backButton.pop();
    }
});

function editHashAndScroll(hash, dontScroll, lazyScroll) {
    var backToPrevY = function () {
        var y = window.pageYOffset;
        return function () {
            window.scrollTo(0, y);
        };
    }();

    var elmt = document.getElementById(hash.slice(1));
    var x = dontScroll? 0 : (function () {
        var rect = elmt.getBoundingClientRect();
        var h = rect.bottom - rect.top;

        if (rect.top < 0 || h > window.innerHeight) return -1;
        // rect.top >= 0 && h <= window.innerHeight

        if (rect.bottom <= window.innerHeight) return 0;
        // rect.bottom > window.innerHeight && h <= window.innerHeight

        return lazyScroll? 1 : -1;
    })();

    window.location.hash = hash;

    switch (x) {
    case 0:
        backToPrevY();
        break;
    case -1:
        elmt.scrollIntoView(true);
        break;
    case 1:
        elmt.scrollIntoView(false);
    }
}

window.addEventListener("click", function (e) {
    var id = getEnclosingID(e.target);
    var elmt;

    if (!id) return;
    // id is truthy

    elmt = document.getElementById(id);

    if (id === "share-button") {
        overlay.setContent(shareButton.getContent());
        overlay.show();
        return;
    }

    // if the click is originated from an on screen element,
    // prevent page from scrolling after location.hash update
    if (e.target.tagName !== "A") {
        editHashAndScroll(id, true);
        shareButton.showAt(elmt.getBoundingClientRect().top);
        shareButton.setContent(
            elmt.textContent,
            window.location.href);
        return;
    } // e.target.tagName === "A"

    e.preventDefault();
    if (id !== "back-button") {
        backButton.push(id);
    }
    editHashAndScroll(
        e.target.getAttribute("href"),
        false,
        id==="back-button");
});
