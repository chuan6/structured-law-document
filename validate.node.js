const fs = require('fs');
const dom = require('jsdom');

function domText(elmt, removeFn) {
  var s = '';
  var cs = elmt.childNodes, i, c;

  for (i = 0; i < cs.length; i++) {
    c = cs[i];

    if (removeFn(c)) continue;

    if (c.nodeType === 3) {
      s += c.textContent;
    } else {
      s += domText(c, removeFn);
    }
  }
  return s;
}

function withClassPred(className) {
  return function (elmt) {
    var flag = false, cs = elmt.classList, i;

    if (!cs) return flag;

    for (i =0; i < cs.length; i++) {
      flag = flag || cs[i] === className;
    }
    return flag;
  };
}

function doSkip(c) {
  return c===' ' || c==='\r' || c==='\n' || c==='\u3000';
}

function norm(s) {
  var cv, c, d, i;

  cv = Array.from(s);
  for (i = 0; i < cv.length; i++) {
    c = cv[i];
    switch (c) {
    case '(':
      d = '（'; break;
    case ')':
      d = '）'; break;
    case ':':
      d = '：'; break;
    case ',':
      d = '，'; break;
    case ';':
      d = '；'; break
    default:
      d = doSkip(c)? '' : c;
      break;
    }
    cv[i] = d;
  }
  return cv.join('');
}

function getTextFromHTML(w) {
  var mainNode = w.document.querySelector('.entries-container'),
      ignore = withClassPred('not-in-original-text');
  return domText(mainNode, ignore);
}

function normCmp(a, b, name) {
  if (norm(a) === norm(b)) {
    console.log('pass', name);
  } else {
    console.error('fail', name);
  }
};

function makeCallback(f, y, z) {
  return function (err, x) {
    if (err) throw err;
    f(x, y, z);
  };
}

function readTXTAndCompare(w, name) {
  var fromHTML = getTextFromHTML(w);

  fs.readFile(
    'generator/resources/' + name + '.txt',
    'utf8',
    makeCallback(normCmp, fromHTML, name)
  );
}

function goThroughRefs(w) {
  var entries, ref, i;

  entries = w.document.querySelectorAll('.entry a');
  for (i = 0; i < entries.length; i++) {
    ref = entries[i];
    dom.env({
      file: ref.getAttribute('href'),
      done: makeCallback(readTXTAndCompare, ref.textContent)
    });
  }
}

dom.env('index.html', makeCallback(goThroughRefs));
