var lastTouch;

var touchState = {
	touch1: null, //see below
	touch2: null, //see below
	s0:  1, // Float
	s:   1, // Float
	t0x: 0, // Float
	t0y: 0, // Float
	tx:  0, // Float
	ty:  0, // Float
	target: null,
}
//var touchN = {
//	id: touch.identifier, // Int
//	x:  touch.pageX, // Float
//	x0: touch.pageX, // Float
//	y:  touch.pageY, // Float
//	y0: touch.pageY  // Float
//}

window.onload = startup;

function startup() {
  var el = document.getElementsByTagName("svg")[0];
  el.addEventListener("touchstart", handleStart, false);
  el.addEventListener("touchend", handleEnd, false);
  el.addEventListener("touchcancel", handleEnd, false);
  el.addEventListener("touchleave", handleEnd, false);
  el.addEventListener("touchmove", handleMove, false);
  touchState.target = el;
  console.log("initialized.");
}

function handleStart(e) {
	e.preventDefault();
  e.stopImmediatePropagation();
	console.log("Start:");
	lastTouch = e;
	for(i=0; i< e.changedTouches.length; i++){
		touch = e.changedTouches[i];
		if(!touchState.touch1){
			commitTransformation();
			touchState.touch1 = {
				id: touch.identifier,
				x:  touch.pageX,
				x0: touch.pageX,
				y:  touch.pageY,
				y0: touch.pageY
			}
		}
		else if(!touchState.touch2){
			commitTransformation();
			touchState.touch2 = {
				id: touch.identifier,
				x:  touch.pageX,
				x0: touch.pageX,
				y:  touch.pageY,
				y0: touch.pageY
			}

		}
	}
}

function handleEnd(e) {
	e.preventDefault();
  e.stopImmediatePropagation();
	console.log("End");
	for(i=0; i< e.changedTouches.length; i++){
		touch = e.changedTouches[i];
		if(touchState.touch1 && touchState.touch1.id == touch.identifier){
			commitTransformation();
			touchState.touch1 = touchState.touch2;
			touchState.touch2 = null;

		}
		else if(touchState.touch2 && touchState.touch2.id == touch.identifier){
			commitTransformation();
			touchState.touch2 = null;
		}
	}
}
function calcTransformation() {
	if(touchState.touch2){
		diffx0 = touchState.touch2.x0 - touchState.touch1.x0
		diffy0 = touchState.touch2.y0 - touchState.touch1.y0
		diffx  = touchState.touch2.x  - touchState.touch1.x
		diffy  = touchState.touch2.y  - touchState.touch1.y
		s = (diffx0 * diffx + diffy0 * diffy)/(diffx0*diffx0 + diffy0*diffy0)
		touchState.tx = touchState.t0x + touchState.s0 * (touchState.touch1.x-s*touchState.touch1.x0+touchState.touch2.x-s*touchState.touch2.x0)/2
		touchState.ty = touchState.t0y + touchState.s0 * (touchState.touch1.y-s*touchState.touch1.y0+touchState.touch2.y-s*touchState.touch2.y0)/2
		touchState.s  = s * touchState.s0
	}else if(touchState.touch1){
		touchState.tx = touchState.t0x + touchState.touch1.x - touchState.touch1.x0
		touchState.ty = touchState.t0y + touchState.touch1.y - touchState.touch1.y0
	}
	touchState.target.setAttribute("transform",transformString());
}
function commitTransformation() {
	if(touchState.touch1){
		touchState.touch1.x0 = touchState.touch1.x
		touchState.touch1.y0 = touchState.touch1.y
	}
	if(touchState.touch2){
		touchState.touch2.x0 = touchState.touch2.x
		touchState.touch2.y0 = touchState.touch2.y
	}
	touchState.s0 = touchState.s
	touchState.t0x = touchState.tx
	touchState.t0y = touchState.ty
}

function handleMove(e) {
	e.preventDefault();
  e.stopImmediatePropagation();
	for(i=0; i< e.changedTouches.length; i++){
		touch = e.changedTouches[i];
		if(touchState.touch1 && touchState.touch1.id == touch.identifier){
			touchState.touch1.x = touch.pageX;
			touchState.touch1.y = touch.pageY;
		}
		else if(touchState.touch2 && touchState.touch2.id == touch.identifier){
			touchState.touch2.x = touch.pageX;
			touchState.touch2.y = touch.pageY;
		}
	}
	calcTransformation();
	console.log(transformString());
}
function transformString() {
	return "translate(" + touchState.tx +", " + touchState.ty + ") scale(" + touchState.s + ")";
}