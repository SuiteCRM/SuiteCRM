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

namespace Google\Service\MapsPlaces;

class GoogleMapsPlacesV1RouteModifiers extends \Google\Model
{
  /**
   * @var bool
   */
  public $avoidFerries;
  /**
   * @var bool
   */
  public $avoidHighways;
  /**
   * @var bool
   */
  public $avoidIndoor;
  /**
   * @var bool
   */
  public $avoidTolls;

  /**
   * @param bool
   */
  public function setAvoidFerries($avoidFerries)
  {
    $this->avoidFerries = $avoidFerries;
  }
  /**
   * @return bool
   */
  public function getAvoidFerries()
  {
    return $this->avoidFerries;
  }
  /**
   * @param bool
   */
  public function setAvoidHighways($avoidHighways)
  {
    $this->avoidHighways = $avoidHighways;
  }
  /**
   * @return bool
   */
  public function getAvoidHighways()
  {
    return $this->avoidHighways;
  }
  /**
   * @param bool
   */
  public function setAvoidIndoor($avoidIndoor)
  {
    $this->avoidIndoor = $avoidIndoor;
  }
  /**
   * @return bool
   */
  public function getAvoidIndoor()
  {
    return $this->avoidIndoor;
  }
  /**
   * @param bool
   */
  public function setAvoidTolls($avoidTolls)
  {
    $this->avoidTolls = $avoidTolls;
  }
  /**
   * @return bool
   */
  public function getAvoidTolls()
  {
    return $this->avoidTolls;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1RouteModifiers::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1RouteModifiers');
