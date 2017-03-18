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
      d = '；'; break;
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
      ignore = withClassPred('n-i-o-t');
  return domText(mainNode, ignore);
}

function samePrefix(va, vb) {
  var i, n = Math.min(va.length, vb.length);

  for (i = 0; i < n; i++) {
    if (va[i] !== vb[i]) break;
  }
  return i;
}

function sameSuffix(va, vb) {
  var c, i, j;
  var na = va.length, nb = vb.length, n = Math.min(na, nb);

  for (c = 0, i = na-1, j = nb-1;
       c < n;
       c++, i--, j--) {

    if (va[i] !== vb[j]) break;

  }
  return c;
}

function normCmp(a, b, name) {
  var va = Array.from(norm(a)),
      vb = Array.from(norm(b));
  var prelen = samePrefix(va, vb),
      suflen = sameSuffix(va, vb),
      na = va.length,
      nb = vb.length;
  var da = na - prelen - suflen;

  if (na === nb && nb === prelen && prelen === suflen) {
    console.log('pass', name);
  } else {
    console.log('fail',
                // mismatched range
                '[' + prelen + ', ' + (na-suflen) + ')\t',
                // length of the mismatched range
                da + '\t',
                // 1st mismatch from front
                va[prelen], '<>', vb[prelen] + '\t',
                // 1st mismatch from behind
                va[na-suflen-1], '<>', vb[nb-suflen-1] + '\t',
                name);
  }
};

function makeCallback(f, y, z) {
  return function (err, x) {
    if (err) throw err;
    f(x, y, z);
  };
}

var rootDir = '../'

function readTXTAndCompare(w, name) {
  var fromHTML = getTextFromHTML(w);

  fs.readFile(
    rootDir + 'generator/resources/' + name + '.txt',
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
      file: rootDir + ref.getAttribute('href'),
      done: makeCallback(readTXTAndCompare, ref.textContent)
    });
  }
}

dom.env(rootDir + 'index.html', makeCallback(goThroughRefs));
