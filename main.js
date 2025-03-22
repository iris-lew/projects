
projects_grid = document.querySelector('#projects_grid_id');
cv = document.querySelector('#cv_describe_id');
isClicked=true;

// function toggle_func() {
//     if (isClicked){
//         projects_grid.style.display="None";
//         cv.style.display="block";
//         isClicked = false;
//     } else{
//         projects_grid.style.display="inline-grid";
//         cv.style.display="None";
//         isClicked = true;
//     }
    
//   }

function openContent(evt, ContentName) {
    // Declare all variables
    var i, tabcontent, tablinks;
  
    // Get all elements with class="tabcontent" and hide them
    tabcontent = document.getElementsByClassName("tabcontent");
    for (i = 0; i < tabcontent.length; i++) {
      tabcontent[i].style.display = "none";
    }
  
    // Get all elements with class="tablinks" and remove the class "active"
    tablinks = document.getElementsByClassName("tablinks");
    for (i = 0; i < tablinks.length; i++) {
      tablinks[i].className = tablinks[i].className.replace(" active", "");
    }
  
    // Show the current tab, and add an "active" class to the button that opened the tab
    document.getElementById(ContentName).style.display = "block";
    evt.currentTarget.className += " active";
  }
document.getElementById("defaultOpen").click();