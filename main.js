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

// Get the modal
var modal = document.getElementById("myModal");
// Get the image and insert it inside the modal - use its "alt" text as a caption
var modalImgs = document.getElementsByClassName('myImages')
var modalImg = document.getElementById("img01");
var captionText = document.getElementById("caption");

// Go through all of the images with our custom class
for (var i = 0; i < modalImgs.length; i++) {
  var imag = modalImgs[i];
  // and attach our click listener for this image.
  imag.onclick = function(evt) {
    modal.style.display = "block";
    modalImg.src = this.src;
    captionText.innerHTML = this.alt;
  }
}

/* ORIGINAL. WORKS
img.onclick = function(){
  modal.style.display = "block";
  modalImg.src = this.src;
  captionText.innerHTML = this.alt;
}
  */

// Get the <span> element that closes the modal
var span = document.getElementsByClassName("close")[0];

// When the user clicks on <span> (x), close the modal
span.onclick = function() {
  modal.style.display = "none";
} 

// image gallery
//https://owlcation.com/stem/javascript_gallery
