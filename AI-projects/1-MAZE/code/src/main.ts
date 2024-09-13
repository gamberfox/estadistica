// Get references to the DOM elements
const fileInput = document.getElementById('fileInput') as HTMLInputElement;
const fileContent = document.getElementById('fileContent') as HTMLElement;
const fileCategorySelect = document.getElementById('fileCategory') as HTMLSelectElement;
const actionSelect = document.getElementById('action') as HTMLSelectElement;

const drawButton=document.getElementById('drawButton') as HTMLButtonElement;
const solveButton = document.getElementById('solveButton') as HTMLButtonElement;
const visualizeButton = document.getElementById('visualizeButton') as HTMLButtonElement;


const mapInfo = document.getElementById('mapInfo') as HTMLPreElement | null;

//delete????????????
//const answer=document.getElementById('mapInfo') as HTMLPreElement;

const map=document.getElementById('grid') as HTMLElement;

const nutcrackerAudio:HTMLAudioElement = document.getElementById('nutcracker') as HTMLAudioElement;


//create map
for (let i = 0; i < 10; i++) {
    for (let j = 0; j < 10; j++) {
      const cell = document.createElement('div');
      cell.classList.add('cell0');
      cell.setAttribute('data-x', j.toString());
      cell.setAttribute('data-y', i.toString());

      map?.appendChild(cell);
    }
  }

const testVar:number=2;

let robot:Robot;

let mapWasCreated:boolean=false;

// Define action options for each category
type ActionOption = { value: string; text: string };
type ActionOptionsMap = {
    informedAlgorithm: ActionOption[];
    notInformedAlgorithm: ActionOption[];
};
const actionOptions: { [key: string]: { value: string; text: string }[] } = {  
    informedAlgorithm: [
        { value: 'avara', text: 'avara' },
        { value: 'aStar', text: 'A star' }
    ],
    notInformedAlgorithm: [
        { value: 'breadth', text: 'breadth' },
        { value: 'uniformCost', text: 'uniform cost' },
        { value: 'depthAvoidingCycles', text: 'depth avoiding cycles' }
    ]
};

// Function to update the second dropdown based on the file category
const updateActionOptions = (category: string) => {
    actionSelect.innerHTML = ''; // Clear current options
    const options = actionOptions[category];
    options.forEach(option => {
        const opt = document.createElement('option');
        opt.value = option.value;
        opt.textContent = option.text;
        actionSelect.appendChild(opt);
    });
};

// Listen for changes in the file category (first panel)
fileCategorySelect.addEventListener('change', (event) => {
    const selectedCategory = (event.target as HTMLSelectElement).value;
    updateActionOptions(selectedCategory);
});

drawButton.addEventListener('click',()=>{
    const selectedAction = actionSelect.value; // Get the selected action
    const file = fileInput.files?.[0]; // Get the selected file
    console.log(`here in the drawButton`);
    
    if(file){
        const reader = new FileReader();
        reader.onload = (e) => {
            //fileContent.textContent = e.target?.result as string; // Display the file content
            parseMap(e.target?.result as string);
            drawMap();
        };
        reader.readAsText(file);
    }
    else{
        fileContent.textContent = 'No file selected.';
    }
});

