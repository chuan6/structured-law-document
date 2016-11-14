// See:
// https://mathiasbynens.be/notes/javascript-unicode#iterating-over-symbols
// prefer for...of iterator; but it is currently not supported by Weixin's
// X5 kernel by Tencent.
function getSymbols(string) {
  var index = 0;
  var length = string.length;
  var output = [];
  for (; index < length - 1; ++index) {
    var charCode = string.charCodeAt(index);
    if (charCode >= 0xD800 && charCode <= 0xDBFF) {
      charCode = string.charCodeAt(index + 1);
      if (charCode >= 0xDC00 && charCode <= 0xDFFF) {
	output.push(string.slice(index, index + 2));
	++index;
	continue;
      }
    }
    output.push(string.charAt(index));
  }
  output.push(string.charAt(index));
  return output;
}

function strSlice(s, end) {
  var s1 = '', s2 = '';
  var c, i = 0;

  s = getSymbols(s);
  for(i = 0; i < s.length; i++) {
    c = s[i];
    if (i < end) {
      s1 += c;
    } else {
      s2 += c;
    }
  }

  return [s1, s2];
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

function backButtonClosure(elmt) {
  var stack = [];

  var top = function () {
    return stack[stack.length - 1];
  };

  var updateHref = function (id) {
    if (id === "" || id) {
      elmt.href = "#" + id;
    } else {
      elmt.removeAttribute("href");
    }
  };

  updateHref("");
  stack = [{id: "", y: 0}];

  return {
    element: elmt,
    peek: top,
    push: function (id, y) {
      var curr = top();

      // only push new id that is different from the top
      if (!curr || curr.id !== id) {
        stack.push({"id": id, "y": y});
        updateHref(id);
      }
    },
    pop: function () {
      var curr;

      if (stack.length > 1) {
        stack.pop();
      }

      curr = top();
      updateHref(curr? curr.id : "");
    }
  };
}

function shareButtonClosure(elmt) {
  var text, link;

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
    elmt.addEventListener("click", function(e) {
      if (tap.end(e, false)) handler(e);
    });
    elmt.addEventListener("touchstart", function(e) {
      var id, srcElmt;

      console.log("touchstart", Date.now());
      tap.start(e, true);

      id = getEnclosingID(e.target);
      if (id) {
        srcElmt = document.getElementById(id);
        srcElmt.style.boxShadow = "inset 0 0 0.5em silver";
      }
    });
    elmt.addEventListener("touchmove", function(e) {
      console.log("touchmove", Date.now());
      tap.move(e);
    });
    elmt.addEventListener("touchend", function(e) {
      var id, srcElmt;

      console.log("touchend", Date.now());
      if (tap.end(e, true)) {
         if (doPreventDefault) e.preventDefault();
         handler(e);
      }

      id = getEnclosingID(e.target);
      if (id) {
        srcElmt = document.getElementById(id);
        srcElmt.style.boxShadow = null;
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
    document.getElementById("back-button"));

  shareButton = shareButtonClosure(
    document.getElementById("share-button"));

  overlay = overlayClosure(
    document.getElementById("overlay"),
    document.getElementById("share-text"),
    document.getElementById("cancel-overlay"),
    document.getElementById("do-copy"));
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
  var s = "", cs, i;

  if (x.nodeType === 3) {
    return x.textContent;
  }

  if (x.tagName === "P") {
    return x.textContent + "|";
  }

  if (x instanceof HTMLElement) {
    cs = x.childNodes;

    if (cs.length === 0) return s;
    // cs.length > 0

    for (i = 0; i < cs.length; i++) {
      s += textContent(cs[i]);
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
        console.log("elmtOnTarget: set", Date.now());
        editHashAndScroll("#" + id, true);
        shareButton.showAt(elmt.getBoundingClientRect().top);
        shareButton.setContent(
          textContent(elmt),
          window.location.href);
        targetID = id;
        console.log("elmtOnTarget: finished", Date.now());
      }
    }
  };
})();


function tapHandler(e) {
  var src = e.target, id = getEnclosingID(src), elmt, hash;
  var isInPageAnchor = function (s) {
    var a = s.split("://", 2);
    return a.length === 1 && a[0][0] === "#";
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
  if (src.tagName !== "A") {
    elmtOnTarget.update(elmt);
    return;
  } // src.tagName === "A"

  hash = src.getAttribute("href");
  if (isInPageAnchor(hash)) {
    e.preventDefault();
    if (id === "back-button") {
      editHashAndScroll(hash, true);
      window.scrollTo(0, backButton.peek().y);
      backButton.pop();
    } else {
      backButton.push(id, window.pageYOffset);
      editHashAndScroll(hash, false);
    }
  } else {
    // src is an <a> element with an external link
    src.click();
  }
}

tapOn(window, tapHandler, true);

var printEvent = window.matchMedia("print");
printEvent.addListener(function (pe) {
  var entries, i, x;

  entries = document.getElementsByClassName("entry");

  if (pe.matches) {
    // add a copy of for each entry-num element so that
    // the entry-num can be print on both hands of a page
    for (i = 0; i < entries.length; i++) {
      x = entries[i].querySelector(".entry-num");
      if (x) entries[i].insertBefore(x.cloneNode(true), x);
    }
  } else {
    // remove the inserted entry-num elements
    for (i = 0; i < entries.length; i++) {
      x = entries[i].querySelector(".entry-num");
      if (x) entries[i].removeChild(x);
    }
  }
});
