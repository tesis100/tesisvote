init = function () {
	loadIntro();
	heading = document.getElementById("heading");
	body = document.getElementById("body");
	loadCodeVerifier();
	candidateIndex = [];
}

setBody = function (content) {
	body.innerHTML = '<p align="center">' + content + '</p>';
}

loadIntro = function () {
	var xhttp = new XMLHttpRequest();
	xhttp.overrideMimeType('text/plain');
	xhttp.onreadystatechange = function () {
		document.getElementById("intro").innerHTML = xhttp.responseText;
	}
	xhttp.open("GET", "intro.text", true);
	xhttp.send();
}

buildVoteBox = function (candidate) {
	var cell = document.getElementById("vote-" + candidate.NominationID);
	if (candidate.Vote == 0)
		cell.innerHTML = '<input type="button" onclick="voteButton(' + candidate.NominationID + ');" value="Vote">'
	else 
		cell.innerHTML = '<voteNm>' + candidate.Vote + '</voteNm><br>' +
		'<input type="button" onclick="modifyVote(' + candidate.NominationID + ',1);" value="+"><input type="button" onclick="modifyVote(' + candidate.NominationID + ',-1);" value="-"><input type="button" onclick="modifyVote(' + candidate.NominationID + ',0);" value="Clear">';
}

findCandidateFromVote = function (position, vote) {
	var position = candidates.Candidates[position];
	for (var c in position) {
		if (position[c].Vote === vote) return position[c];
	}
}

findCandidateFromNomID = function (nomID) {
	var position = candidates.Candidates[candidateIndex[nomID]];
	for (var c in position) {
		if (parseInt(position[c].NominationID) == nomID) return position[c];
	}
	console.log ("Didn't find candidate: " + nomID);
}

modifyVote = function (nomID, change) {
	var candidate = findCandidateFromNomID (nomID);
	if (change === 1 && candidate.Vote !== 1) {
		var other_candidate = findCandidateFromVote (candidateIndex[nomID], candidate.Vote - 1);
		candidate.Vote = candidate.Vote - 1;
		other_candidate.Vote = other_candidate.Vote + 1;
		buildVoteBox (candidate); buildVoteBox (other_candidate);
	} else if (change === -1 && typeof findCandidateFromVote (candidateIndex[nomID], candidate.Vote + 1) === "object") {
		var other_candidate = findCandidateFromVote (candidateIndex[nomID], candidate.Vote + 1);
		candidate.Vote = candidate.Vote + 1;
		other_candidate.Vote = other_candidate.Vote - 1;
		buildVoteBox (candidate); buildVoteBox (other_candidate);
	} else if (change === 0) {
		var lower_candidate = findCandidateFromVote (candidateIndex[nomID], candidate.Vote + 1);
		if (typeof lower_candidate === "object") modifyVote (lower_candidate.NominationID, 0);
		candidate.Vote = 0;
		buildVoteBox (candidate);
	}
}

voteButton = function (nomID) {
	var positionName = candidateIndex[nomID];
	var position = candidates.Candidates[candidateIndex[nomID]];
	var candidate = undefined;
	var highestVote = 0;
	for (var c in position) {
		if (position[c].Vote > highestVote) highestVote = position[c].Vote;
		if (position[c].NominationID == nomID) candidate = position[c];
	}
	candidate.Vote = highestVote + 1;
	buildVoteBox (candidate);
}

var buildCandidates = function (candidates, position) {
	candidatesHTML = "";
	for (var ci in candidates) {
		var c = candidates[ci];
		candidateIndex[c.NominationID] = position;
		candidatesHTML += '<tr><td id="vote-' + c.NominationID + '"><input type="button" onclick="voteButton(' + c.NominationID + ');" value="Vote"></td><td height="100px"><img src="images/' + c.ShortName + '.jpg"></td><td><p>' + c.Name + '</p></td></tr>'
	}
	return candidatesHTML;
}

var buildPositions = function (positions) {
	var positionsHTML = " ";
	for (var p in positions) {
		positionsHTML += '<tr height="100px"><td width="100px"></td><td width="150px"></td><td style="vertical-align:bottom"><h3>' + p + '</h3></td></tr>';
		positionsHTML += buildCandidates (positions[p], p);
	}
	return positionsHTML;
}

submitVote = function () {
	candidates.Code = code;
	candidates.Type = "Submit";
	connection.onmessage = function (m) {
		console.log ("OnMessage: " + m.data);
		var response = JSON.parse (m.data);
		if (response.Worked === true) {
			alert ("Vote Submitted!");
		} else {
			alert ("Error: " + response.Reason);
		}
	}
	connection.send (JSON.stringify (candidates));
}

loadCandidates = function () {
	heading.innerHTML = "Candidates";
	setBody ("Loading Candidates");
	connection.onmessage = function (m) {
		console.log ("OnMessage: " + m.data);
		candidates = JSON.parse (m.data);
		if (candidates.Type !== "Candidates") {setBody ("ERROR: Wrong Response");}
		else {
			body.innerHTML = '<table>' + buildPositions(candidates.Candidates) + '</table>';
			body.innerHTML += '\n <p align="center"><input type="button" onclick="submitVote();" value="Submit" id="submitButton" class="submitButton"></p>';
		}
	}
	connection.send (JSON.stringify ({Type:"InfoRequest"}))
}

loadCodeVerifier = function () {
	heading.innerHTML = "Enter Code";
	setBody ('<form action="javascript:void(0);" align="center"><input type="text" id="codebox"> <input type="submit" onclick="codeVerifierRun();" value="Submit"></form>');
}
codeVerifierRun = function () {
	code = document.getElementById("codebox").value;
	heading.innerHTML = "Checking Code";
	body.innerHTML = '<p align="center">Checking: ' + code +'</p>';
	connection = new WebSocket('wss://' + window.location.host + '/ws');
	connection.onopen = function () {
		connection.send (JSON.stringify({Type:"Verify",Code:code}));
	}
	connection.onmessage = function (v) {
		console.log ("OnMessage: " + v.data);
		var R = JSON.parse (v.data);
		if (R.Type === "Verify") {
			if (R.Valid === false) {
				setBody ('Code Verify Failed. <input type="button" onclick="location.reload();" value="Retry?">')
			} else {
				setBody ('Code Validity Confirmed');
				setTimeout (loadCandidates, 1000);
				connection.onclose = function (e) {
					console.log ("OnClose: " + e.reason);
					body.innerHTML = '<p align="center">Connection Closed: ' + '<br><input type="button" onclick="location.reload();" value="New Vote">' + "</p>";
				}
			}
		} else throw "Unexpected Message" + v.data;
	}
	connection.onerror = function (e) {
		console.log ("OnError: " + e);
		body.innerHTML = "<p>Connection Failure: " + e + '<br><input type="button" onclick="location.reload();" value="New Vote">' + "</p>";
	}
}