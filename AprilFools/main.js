projects_grid = document.querySelector('#projects_grid_id');
cv = document.querySelector('#cv_describe_id');
isClicked=true;

function toggle_func() {
    if (isClicked){
        projects_grid.style.display="None";
        cv.style.display="block";
        isClicked = false;
    } else{
        projects_grid.style.display="inline-grid";
        cv.style.display="None";
        isClicked = true;
    }
    
  }

  