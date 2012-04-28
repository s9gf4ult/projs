var MyApp = angular.module('MyApp', ['ngCookies']);

MyApp.config(function($routeProvider, $locationProvider) {
    $routeProvider.when('#!project/:name', {template : 'project.html', controller: projectController});
    $routeProvider.when('#!activity/:name', {template: 'acvitity.html', controller: activityController});
    $locationProvider.html5Mode(true);
});

var projectController = function($scope, $route, $routeParams) {
    $scope.name = $routeParams.name;
    $scope.reload = $route.reload;
}

var activityController = function($scope, $routeParams) {
    $scope.name = $routeParams.name;
}

var injector = angular.injector(['ng', 'MyApp']);
