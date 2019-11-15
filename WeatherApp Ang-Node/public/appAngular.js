var app = angular.module('myApp', []);
app.controller('myCtrl', function($scope,$http) {
   $scope.submit= function(){
      var data = JSON.stringify({
            author: $scope.author,
            title : $scope.title,
            body : $scope.body
        })
      console.log(data);
      $http.post("/city", data).then(function(data, status) {
        console.log('Data posted successfully');
      }).catch((err)=>{
          console.log("error");
          console.log(err);
      })
   }

});