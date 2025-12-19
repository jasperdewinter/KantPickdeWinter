const faces = {
  alex: {
    name: "Alex",
    image:
      "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNDAiIGhlaWdodD0iMjQwIiB2aWV3Qm94PSIwIDAgMjQwIDI0MCI+CiAgPHJlY3QgIHdpZHRoPSIyNDAiIGhlaWdodD0iMjQwIiBmaWxsPSIjZjRmMGU2Ii8+CiAgPGNpcmNsZSBjeD0iMTIwIiBjeT0iMTIwIiByPSI5MCIgZmlsbD0iI2YxYzI3ZCIgc3Ryb2tlPSIjZDJhNjc5IiBzdHJva2Utd2lkdGg9IjQiLz4KICA8Y2lyY2xlIGN4PSI5MCIgY3k9IjExMCIgcj0iMTAiIGZpbGw9IiMzYjNiM2IiLz4KICA8Y2lyY2xlIGN4PSIxNTAiIGN5PSIxMTAiIHI9IjEwIiBmaWxsPSIjM2IzYjNiIi8+CiAgPHBhdGggZD0iTTgwIDkwIHEwIC0yMCAyNSAtMjAgcTE1IDAgMjUgMjAiIGZpbGw9IiMxZjRjN2EiLz4KICA8cGF0aCBkPSJNMTEwIDE0NSBxMTAgMTIgMjAgMCIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjYjM1NjFiIiBzdHJva2Utd2lkdGg9IjYiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIvPgogIDxyZWN0IHg9IjkwIiB5PSI3MCIgd2lkdGg9IjYwIiBoZWlnaHQ9IjE2IiByeD0iOCIgZmlsbD0iIzI2MzIzOCIvPgo8L3N2Zz4K",
  },
  maya: {
    name: "Maya",
    image:
      "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNDAiIGhlaWdodD0iMjQwIiB2aWV3Qm94PSIwIDAgMjQwIDI0MCI+CiAgPHJlY3QgIHdpZHRoPSIyNDAiIGhlaWdodD0iMjQwIiBmaWxsPSIjZTdmNWZmIi8+CiAgPGNpcmNsZSBjeD0iMTIwIiBjeT0iMTIwIiByPSI4OCIgc3Ryb2tlLXdpZHRoPSI0IiBmaWxsPSIjZjhkN2IwIiBzdHJva2U9IiNkYmE3N2IiLz4KICA8ZWxsaXBzZSBjeD0iOTAiIGN5PSIxMTIiIHJ4PSIxMSIgcnk9IjkiIGZpbGw9IiMyZDFiMWIiLz4KICA8ZWxsaXBzZSBjeD0iMTUwIiBjeT0iMTEyIiByeD0iMTEiIHJ5PSI5IiBmaWxsPSIjMmQxYjFiIi8+CiAgPHBhdGggZD0iTTcyIDExOCBxMjUgMzIgOTYgMCIgZmlsbD0iI2VlYzFhOCIvPgogIDxwYXRoIGQ9Ik05MiAxNTAgcTI4IDI0IDU2IDAiIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzhlM2I0NiIgc3Ryb2tlLXdpZHRoPSI2IiBzdHJva2UtbGluZWNhcD0icm91bmQiLz4KICA8cGF0aCBkPSJNNzAgODAgcTUwIC0zMCAxMDAgMCIgZmlsbD0iIzdCM0YwMCIvPgogIDxyZWN0IHg9IjcwIiB5PSI3NSIgd2lkdGg9IjEwMCIgaGVpZ2h0PSIyNSIgZmlsbD0iIzdCM0YwMCIgcng9IjEwIi8+Cjwvc3ZnPgo=",
  },
  jordan: {
    name: "Jordan",
    image:
      "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNDAiIGhlaWdodD0iMjQwIiB2aWV3Qm94PSIwIDAgMjQwIDI0MCI+CiAgPHJlY3QgIHdpZHRoPSIyNDAiIGhlaWdodD0iMjQwIiBmaWxsPSIjZWVmN2VkIi8+CiAgPGNpcmNsZSBjeD0iMTIwIiBjeT0iMTIwIiByPSI5MiIgc3Ryb2tlLXdpZHRoPSI0IiBmaWxsPSIjZDliMzhjIiBzdHJva2U9IiNiNTg5NjMiLz4KICA8ZWxsaXBzZSBjeD0iOTAiIGN5PSIxMTIiIHJ4PSIxMCIgcnk9IjEwIiBmaWxsPSIjMWIyYjM0Ii8+CiAgPGVsbGlwc2UgY3g9IjE1MCIgY3k9IjExMiIgcng9IjEwIiByeT0iMTAiIGZpbGw9IiMxYjJiMzQiLz4KICA8cGF0aCBkPSJNODEgODQgcTM2IC0yOCA3MiAwIiBmaWxsPSIjM2EyNzE4Ii8+CiAgPHJlY3QgeD0iODgiIHk9Ijc4IiB3aWR0aD0iNjQiIGhlaWdodD0iMzAiIHJ4PSI2IiBmaWxsPSIjM2EyNzE4Ii8+CiAgPHBhdGggZD0iTTkyIDE1MiBxMjggMTAgNTYgMCIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjNGYyNTAwIiBzdHJva2Utd2lkdGg9IjYiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIvPgogIDxwYXRoIGQ9Ik0xMTYgMTM0IHE0IDggOCAwIiBmaWxsPSJub25lIiBzdHJva2U9IiM0ZjI1MDAiIHN0cm9rZS13aWR0aD0iNCIvPgo8L3N2Zz4K",
  },
  skylar: {
    name: "Skylar",
    image:
      "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNDAiIGhlaWdodD0iMjQwIiB2aWV3Qm94PSIwIDAgMjQwIDI0MCI+CiAgPHJlY3QgIHdpZHRoPSIyNDAiIGhlaWdodD0iMjQwIiBmaWxsPSIjZjJmN2ZmIi8+CiAgPGNpcmNsZSBjeD0iMTIwIiBjeT0iMTIwIiByPSI5MCIgc3Ryb2tlLXdpZHRoPSI0IiBmaWxsPSIjZjNjOWE5IiBzdHJva2U9IiNkOWEzN2EiLz4KICA8ZWxsaXBzZSBjeD0iOTAiIGN5PSIxMDgiIHJ4PSI5IiByeT0iOSIgc3Ryb2tlLXdpZHRoPSIwIiBmaWxsPSIjMWQyNDMzIi8+CiAgPGVsbGlwc2UgY3g9IjE1MCIgY3k9IjEwOCIgcng9IjkiIHJ5PSI5IiBzdHJva2Utd2lkdGg9IjAiIGZpbGw9IiMxZDI0MzMiLz4KICA8cGF0aCBkPSJNMTIwIDYwIGwtMTIgMzAgaDI0eiIgZmlsbD0iIzFkMjQzMyIvPgogIDxwYXRoIGQ9Ik03MCA5NSBxNTAgLTQ0IDEwMCAwIiBmaWxsPSIjYzk5YzZmIi8+CiAgPHBhdGggZD0iTTY4IDk1IHE1MiAyNCAxMDQgMCIgZmlsbD0iI2M5OWM2ZiIvPgogIDxwYXRoIGQ9Ik05MiAxNTAgcTI4IC0xMiA1NiAwIiBmaWxsPSJub25lIiBzdHJva2U9IiM3ZDJmNDUiIHN0cm9rZS13aWR0aD0iNiIgc3Rya2UtbGluZWNhcD0icm91bmQiLz4KICA8cGF0aCBkPSJNMTEyIDEzMiBxOCA4IDE2IDAiIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzdkMmY0NSIgc3Rya2Utd2lkdGg9IjQiLz4KPC9zdmc+Cg==",
  },
};

const questions = [
  {
    id: "morph-1",
    prompt: "Which face is hidden beneath this morph?",
    base: "alex",
    blend: "maya",
    answer: "Maya",
    options: ["Maya", "Jordan", "Skylar", "Alex"],
  },
  {
    id: "morph-2",
    prompt: "Slide to reveal the mix and guess the person!",
    base: "jordan",
    blend: "skylar",
    answer: "Jordan",
    options: ["Skylar", "Jordan", "Alex", "Maya"],
  },
  {
    id: "morph-3",
    prompt: "Who shines through this blend?",
    base: "maya",
    blend: "skylar",
    answer: "Skylar",
    options: ["Skylar", "Jordan", "Maya", "Alex"],
  },
];

const slider = document.getElementById("morph-slider");
const sliderValue = document.getElementById("slider-value");
const baseFace = document.getElementById("base-face");
const blendFace = document.getElementById("blend-face");
const promptEl = document.getElementById("prompt");
const optionsEl = document.getElementById("options");
const feedbackEl = document.getElementById("feedback");
const nextBtn = document.getElementById("next-btn");

let currentIndex = 0;

function setMorphOpacity(value) {
  sliderValue.textContent = value;
  blendFace.style.opacity = value / 100;
}

function shuffle(array) {
  const clone = [...array];
  for (let i = clone.length - 1; i > 0; i -= 1) {
    const j = Math.floor(Math.random() * (i + 1));
    [clone[i], clone[j]] = [clone[j], clone[i]];
  }
  return clone;
}

function renderQuestion() {
  const question = questions[currentIndex];
  promptEl.textContent = question.prompt;

  const base = faces[question.base];
  const blend = faces[question.blend];
  baseFace.src = base.image;
  blendFace.src = blend.image;

  setMorphOpacity(50);
  slider.value = 50;
  feedbackEl.textContent = "";
  feedbackEl.className = "feedback";

  optionsEl.innerHTML = "";
  const shuffledOptions = shuffle(question.options);
  shuffledOptions.forEach((label) => {
    const button = document.createElement("button");
    button.type = "button";
    button.className = "option-button";
    button.textContent = label;
    button.addEventListener("click", () => handleGuess(button, label));
    optionsEl.appendChild(button);
  });
}

function handleGuess(button, guess) {
  const question = questions[currentIndex];
  const buttons = Array.from(optionsEl.querySelectorAll("button"));
  buttons.forEach((b) => {
    b.disabled = true;
  });

  const isCorrect = guess === question.answer;
  if (isCorrect) {
    feedbackEl.textContent = `Nice! The morph hides ${question.answer}.`;
    feedbackEl.className = "feedback correct";
    button.classList.add("correct");
  } else {
    feedbackEl.textContent = `Not quite. The hidden face is ${question.answer}.`;
    feedbackEl.className = "feedback incorrect";
    button.classList.add("incorrect");
    const correctButton = buttons.find((b) => b.textContent === question.answer);
    if (correctButton) {
      correctButton.classList.add("correct");
    }
  }
}

function nextQuestion() {
  currentIndex = (currentIndex + 1) % questions.length;
  renderQuestion();
}

slider.addEventListener("input", (event) => {
  setMorphOpacity(event.target.value);
});

nextBtn.addEventListener("click", nextQuestion);

renderQuestion();
