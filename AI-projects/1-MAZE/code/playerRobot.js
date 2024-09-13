class Robot {
    constructor(x, y, gridWidth, gridHeight) {
      this.x = x;
      this.y = y;
      this.gridWidth = gridWidth;
      this.gridHeight = gridHeight;
      this.gridElement = document.getElementById('grid');
      this.createGrid();
      this.updateGrid();
      this.addKeyboardControls(); // Add keyboard controls
    }
  
    createGrid() {
      for (let i = 0; i < this.gridHeight; i++) {
        for (let j = 0; j < this.gridWidth; j++) {
          const cell = document.createElement('div');
          cell.classList.add('cell');
          cell.setAttribute('data-x', j);
          cell.setAttribute('data-y', i);
  
          this.gridElement.appendChild(cell);
        }
      }
    }
  
    updateGrid() {
      document.querySelectorAll('.cell').forEach(cell => {
        cell.classList.remove('robot');
      });
      const robotCell = document.querySelector(`.cell[data-x='${this.x}'][data-y='${this.y}']`);
      if (robotCell) {
        robotCell.classList.add('robot');
      }
    }
  
    moveUp() {
      if (this.y > 0) {
        this.y -= 1;
        this.updateGrid();
      } else {
        alert("Can't move up, at top boundary.");
      }
    }
  
    moveDown() {
      if (this.y < this.gridHeight - 1) {
        this.y += 1;
        this.updateGrid();
      } else {
        alert("Can't move down, at bottom boundary.");
      }
    }
  
    moveLeft() {
      if (this.x > 0) {
        this.x -= 1;
        this.updateGrid();
      } else {
        alert("Can't move left, at left boundary.");
      }
    }
  
    moveRight() {
      if (this.x < this.gridWidth - 1) {
        this.x += 1;
        this.updateGrid();
      } else {
        alert("Can't move right, at right boundary.");
      }
    }
  
    addKeyboardControls() {
      // Add event listener for keydown events
      document.addEventListener('keydown', (event) => {
        switch (event.key) {
          case 'ArrowUp':
            this.moveUp();
            break;
          case 'ArrowDown':
            this.moveDown();
            break;
          case 'ArrowLeft':
            this.moveLeft();
            break;
          case 'ArrowRight':
            this.moveRight();
            break;
          default:
            break;
        }
      });
    }
  }
  
  // Initialize the robot
  const robot = new Robot(0, 0, 5, 5); // Starting position (0, 0) on a 5x5 grid
  