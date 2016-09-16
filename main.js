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

    stack = [{"id": init, "y": 0}];

    top = function () {
        return stack[stack.length - 1];
    };

    return {
        "element": elmt,
        "push": function (id, y) {
            console.log("before push:", stack);
            if (id !== top().id) {
                // only push new id that is different from the top
                stack.push({"id": id, "y": y});
                updateHrefToID(elmt, id);
            }
            console.log("after push:", stack);
        },
        "pop": function () {
            console.log("before pop:", stack);
            if (stack.length > 1) {
                stack.pop();
            }
            updateHrefToID(elmt, top().id);
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
        "clear": function () {
            text = link = null;
            elmt.style.display = "none";
        },
        "setContent": function (s, ref) {
            text = s;
            link = ref;
        },
        "getContent": function () {
            var nchars = 64;
            var sliced = strSlice(text, nchars);
            return sliced[0] + (sliced[1]? "……":"") + " " + link;
        }
    };
}

var tapOn = (function () {
    var tap = (function () {
        var status = 0; //0 - initial; 1 - started; 2 - moved
        var x, y;

        return {
            // Return true if the op is accepted as part of a tap;
            // return false if otherwise.
            "start": function(e, isTouch) {
                if (status === 0) {
                    status = 1;
                    if (!isTouch) {
                        x = e.layerX;
                        y = e.layerY;
                    }
                    return true;
                } else {
                    return false;
                }
            },
            "move": function(e) {
                if (status === 1) {
                    status = 2;
                    return true;
                } else {
                    return false;
                }
            },
            "end": function(e, isTouch) {
                if (status === 1) {
                    status = 0;
                    if (!isTouch && (x !== e.layerX || y !== e.layerY)) {
                        return false;
                    }
                    return true;
                } else {
                    status = 0;
                    return false;
                }
            }
        };
    }());

    return function(elmt, handler, doPreventDefault) {
        elmt.addEventListener("mousedown", function(e) {
            tap.start(e, false);
        });
        elmt.addEventListener("mouseup", function(e) {
            if (tap.end(e, false)) handler(e);
        });
        elmt.addEventListener("touchstart", function(e) {
            console.log("touchstart");
            tap.start(e, true);
        });
        elmt.addEventListener("touchmove", function(e) {
            console.log("touchmove");
            tap.move(e);
        });
        elmt.addEventListener("touchend", function(e) {
            console.log("touchend");
            if (tap.end(e, true)) {
                if (doPreventDefault) e.preventDefault();
                handler(e);
            }
        });
    };
}());

function overlayClosure(elmt, content, docancel, docopy) {
    var computed, textareaWidth;


    tapOn(elmt, function (e) {
        e.stopPropagation();
    }, false);

    tapOn(docancel, function (e) {
        console.log("docancel");
        elmt.style.display = "none";
        content.removeAttribute("readonly");
        e.stopPropagation();
    }, true);

    tapOn(docopy, function (e) {
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
    }, true);

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
    var bbtop = backButton.peek();

    if (hash === "#" + bbtop.id) {
        window.scrollTo(0, bbtop.y);
        backButton.pop();
    }
});

function editHashAndScroll(hash, dontAutoScroll) {
    var backToPrevY = function () {
        var y = window.pageYOffset;
        return function () {
            window.scrollTo(0, y);
        };
    }();

    var elmt = document.getElementById(hash.slice(1));
    console.assert(elmt || dontAutoScroll);
    var x = dontAutoScroll? 0 : (function () {
        var rect = elmt.getBoundingClientRect();
        var h = rect.bottom - rect.top;

        if (rect.top < 0 || h > window.innerHeight) return -1;
        // rect.top >= 0 && h <= window.innerHeight

        if (rect.bottom <= window.innerHeight) return 0;
        // rect.bottom > window.innerHeight && h <= window.innerHeight

        return 1;
    })();

    window.location.hash = hash;

    console.log(x);

    switch (x) {
    case 0:
        backToPrevY();
        break;
    case -1:
        elmt.scrollIntoView(true);
        break;
    case 1:
        break;
        //elmt.scrollIntoView(false);
    }
}

function textContent(x) {
    var s = "", i, cs;

    if (x.nodeType === 3) {
        return x.textContent;
    }

    if (x instanceof HTMLElement) {
        cs = x.childNodes;

        if (cs.length === 0) {
            return s;
        } // cs.length > 0

        s += textContent(cs[0]);
        for (i = 1; i < cs.length; i++) {
            s += (cs[i].tagName==="P"? "|":"")
                +  textContent(cs[i]);
        }
        return s;
    }

    return null;
}

var elmtOnTarget = (function () {
    var targetID = null;

    return {
        "update": function (elmt) {
            var id = elmt.id;
            console.assert(id, "element passed here must have an ID");

            if (id === targetID) { // clear
                console.log("elmtOnTarget: clear");
                editHashAndScroll("", true);
                shareButton.clear();
                targetID = null;
            } else { // set
                console.log("elmtOnTarget: set");
                editHashAndScroll("#" + id, true);
                shareButton.showAt(elmt.getBoundingClientRect().top);
                shareButton.setContent(
                    textContent(elmt),
                    window.location.href);
                targetID = id;
            }
        }
    };
})();


function tapHandler(e) {
    var id = getEnclosingID(e.target);
    var elmt;
    var isInPageAnchor = function (s) {
        return s.split("://", 2).length === 1;
    };

    console.log("tapped on ", id);

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
        elmtOnTarget.update(elmt);
        return;
    } // e.target.tagName === "A"

    if (isInPageAnchor(e.target.getAttribute("href"))) {
        e.preventDefault();
        if (id !== "back-button") {
            backButton.push(id, window.pageYOffset);
        }
        editHashAndScroll(
            e.target.getAttribute("href"),
            false
        );
    } else {
        // e.target is an <a> element with an external link
        e.target.click();
    }
}

tapOn(window, tapHandler, true);
