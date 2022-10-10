function pageLoad() {
    document.getElementById('practice').style.display = 'none';
    document.getElementById('trials').style.display = 'none';
    document.getElementById('position').style.display = 'none';
    document.getElementById('clockdiv').style.display = "none";
    document.getElementById('consent').style.display = 'block';
}

function clickConsent() {
    document.getElementById('welcome').style.display = 'block';
    document.getElementById('consent').style.display = 'none';
}

function clickInstruction() {
    document.getElementById('welcome').style.display = 'none';
    document.getElementById('instruction').style.display = 'block';

}

function clickPractice() {
    document.getElementById('instruction').style.display = 'none';
    document.getElementById('practice').style.display = 'block';
}

function clickTrials() {
    document.getElementById('practice').style.display = 'none';
    document.getElementById('position').style.display = 'block';
}

