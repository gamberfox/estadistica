"use strict";
// Get references to the DOM elements
const fileInput = document.getElementById('fileInput');
const fileContent = document.getElementById('fileContent');
const fileCategorySelect = document.getElementById('fileCategory');
const actionSelect = document.getElementById('action');
const performActionButton = document.getElementById('performAction');
const actionOptions = {
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
const updateActionOptions = (category) => {
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
    const selectedCategory = event.target.value;
    updateActionOptions(selectedCategory);
});
// Handle button click to perform the selected action
performActionButton.addEventListener('click', () => {
    const selectedAction = actionSelect.value; // Get the selected action
    const file = fileInput.files?.[0]; // Get the selected file
    if (file) {
        const reader = new FileReader(); // Create a new FileReader
        switch (selectedAction) {
            case 'avara':
                // Read and display the file content
                reader.onload = (e) => {
                    fileContent.textContent = e.target?.result; // Display the file content
                };
                reader.readAsText(file); // Read the file as text
                break;
            case 'aStar':
                // Display the file size in bytes
                fileContent.textContent = `File Size: ${file.size} bytes`;
                break;
            case 'breadth':
                // Display the file type (MIME type)
                fileContent.textContent = `File Type: ${file.type || 'Unknown'}`;
                break;
            case 'uniformCost':
                // Preview image (for image files)
                reader.onload = (e) => {
                    const imageUrl = e.target?.result;
                    fileContent.innerHTML = `<img src="${imageUrl}" alt="Preview" style="max-width: 100%;">`;
                };
                reader.readAsDataURL(file); // Read the file as a data URL (for image preview)
                break;
            default:
                fileContent.textContent = 'Please select a valid action.';
                break;
        }
        reader.onerror = () => {
            fileContent.textContent = 'Error reading file!';
        };
    }
    else {
        fileContent.textContent = 'No file selected.';
    }
});
