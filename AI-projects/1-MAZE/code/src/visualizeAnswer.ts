

// Initialize the robot
//const robot1 = new Robot(0, 0, 5, 5); // Starting position (0, 0) on a 5x5 grid

//

const map2=document.getElementById('grid') as HTMLElement;
visualizeButton.addEventListener('click',()=>{
    //robot =new Robot(0,0,5,5,100);
    //let robot =new Robot(0,0,10,10,100);
    console.log(`oioioi\n`);
    //robot.followDirections([3,3,3,3,0]);
    robot.followDirections(answer);
});