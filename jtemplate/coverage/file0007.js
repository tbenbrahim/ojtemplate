var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 234</i>';
states['fold000001'] = false;
texts['fold000236'] = '<a href="javascript:fold(\'fold000236\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 236 to line 248</i>';
states['fold000236'] = false;
texts['fold000252'] = '<a href="javascript:fold(\'fold000252\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 252 to line 252</i>';
states['fold000252'] = false;
texts['fold000254'] = '<a href="javascript:fold(\'fold000254\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 254 to line 260</i>';
states['fold000254'] = false;
texts['fold000262'] = '<a href="javascript:fold(\'fold000262\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 262 to line 271</i>';
states['fold000262'] = false;
texts['fold000273'] = '<a href="javascript:fold(\'fold000273\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 273 to line 275</i>';
states['fold000273'] = false;
texts['fold000277'] = '<a href="javascript:fold(\'fold000277\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 277 to line 279</i>';
states['fold000277'] = false;
texts['fold000281'] = '<a href="javascript:fold(\'fold000281\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 281 to line 284</i>';
states['fold000281'] = false;
texts['fold000286'] = '<a href="javascript:fold(\'fold000286\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 286 to line 289</i>';
states['fold000286'] = false;
texts['fold000292'] = '<a href="javascript:fold(\'fold000292\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 292 to line 295</i>';
states['fold000292'] = false;
texts['fold000298'] = '<a href="javascript:fold(\'fold000298\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 298 to line 301</i>';
states['fold000298'] = false;
texts['fold000304'] = '<a href="javascript:fold(\'fold000304\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 304 to line 304</i>';
states['fold000304'] = false;
texts['fold000307'] = '<a href="javascript:fold(\'fold000307\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 307 to line 321</i>';
states['fold000307'] = false;
texts['fold000323'] = '<a href="javascript:fold(\'fold000323\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 323 to line 350</i>';
states['fold000323'] = false;
texts['fold000352'] = '<a href="javascript:fold(\'fold000352\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 352 to line 365</i>';
states['fold000352'] = false;
texts['fold000372'] = '<a href="javascript:fold(\'fold000372\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 372 to line 383</i>';
states['fold000372'] = false;
texts['fold000385'] = '<a href="javascript:fold(\'fold000385\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 385 to line 428</i>';
states['fold000385'] = false;
texts['fold000430'] = '<a href="javascript:fold(\'fold000430\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 430 to line 430</i>';
states['fold000430'] = false;
texts['fold000433'] = '<a href="javascript:fold(\'fold000433\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 433 to line 433</i>';
states['fold000433'] = false;
texts['fold000435'] = '<a href="javascript:fold(\'fold000435\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 435 to line 436</i>';
states['fold000435'] = false;
texts['fold000438'] = '<a href="javascript:fold(\'fold000438\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 438 to line 452</i>';
states['fold000438'] = false;
texts['fold000454'] = '<a href="javascript:fold(\'fold000454\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 454 to line 455</i>';
states['fold000454'] = false;

function fold(id) {
  tmp = document.all[id].innerHTML;
  document.all[id].innerHTML = texts[id];
  texts[id] = tmp;
  states[id] = !(states[id]);
}

function unfoldAll() {
  for (key in states) {
    if (states[key]) {
      fold(key);
    }
  }
}

function foldAll() {
  for (key in states) {
    if (!(states[key])) {
      fold(key);
    }
  }
}
