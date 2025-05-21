<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\CloudDeploy;

class GatewayServiceMesh extends \Google\Model
{
  /**
   * @var string
   */
  public $deployment;
  /**
   * @var string
   */
  public $httpRoute;
  /**
   * @var string
   */
  public $podSelectorLabel;
  protected $routeDestinationsType = RouteDestinations::class;
  protected $routeDestinationsDataType = '';
  /**
   * @var string
   */
  public $routeUpdateWaitTime;
  /**
   * @var string
   */
  public $service;
  /**
   * @var string
   */
  public $stableCutbackDuration;

  /**
   * @param string
   */
  public function setDeployment($deployment)
  {
    $this->deployment = $deployment;
  }
  /**
   * @return string
   */
  public function getDeployment()
  {
    return $this->deployment;
  }
  /**
   * @param string
   */
  public function setHttpRoute($httpRoute)
  {
    $this->httpRoute = $httpRoute;
  }
  /**
   * @return string
   */
  public function getHttpRoute()
  {
    return $this->httpRoute;
  }
  /**
   * @param string
   */
  public function setPodSelectorLabel($podSelectorLabel)
  {
    $this->podSelectorLabel = $podSelectorLabel;
  }
  /**
   * @return string
   */
  public function getPodSelectorLabel()
  {
    return $this->podSelectorLabel;
  }
  /**
   * @param RouteDestinations
   */
  public function setRouteDestinations(RouteDestinations $routeDestinations)
  {
    $this->routeDestinations = $routeDestinations;
  }
  /**
   * @return RouteDestinations
   */
  public function getRouteDestinations()
  {
    return $this->routeDestinations;
  }
  /**
   * @param string
   */
  public function setRouteUpdateWaitTime($routeUpdateWaitTime)
  {
    $this->routeUpdateWaitTime = $routeUpdateWaitTime;
  }
  /**
   * @return string
   */
  public function getRouteUpdateWaitTime()
  {
    return $this->routeUpdateWaitTime;
  }
  /**
   * @param string
   */
  public function setService($service)
  {
    $this->service = $service;
  }
  /**
   * @return string
   */
  public function getService()
  {
    return $this->service;
  }
  /**
   * @param string
   */
  public function setStableCutbackDuration($stableCutbackDuration)
  {
    $this->stableCutbackDuration = $stableCutbackDuration;
  }
  /**
   * @return string
   */
  public function getStableCutbackDuration()
  {
    return $this->stableCutbackDuration;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GatewayServiceMesh::class, 'Google_Service_CloudDeploy_GatewayServiceMesh');
