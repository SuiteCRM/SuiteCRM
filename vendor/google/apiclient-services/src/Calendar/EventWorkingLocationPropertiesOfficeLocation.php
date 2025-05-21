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

namespace Google\Service\Calendar;

class EventWorkingLocationPropertiesOfficeLocation extends \Google\Model
{
  /**
   * @var string
   */
  public $buildingId;
  /**
   * @var string
   */
  public $deskId;
  /**
   * @var string
   */
  public $floorId;
  /**
   * @var string
   */
  public $floorSectionId;
  /**
   * @var string
   */
  public $label;

  /**
   * @param string
   */
  public function setBuildingId($buildingId)
  {
    $this->buildingId = $buildingId;
  }
  /**
   * @return string
   */
  public function getBuildingId()
  {
    return $this->buildingId;
  }
  /**
   * @param string
   */
  public function setDeskId($deskId)
  {
    $this->deskId = $deskId;
  }
  /**
   * @return string
   */
  public function getDeskId()
  {
    return $this->deskId;
  }
  /**
   * @param string
   */
  public function setFloorId($floorId)
  {
    $this->floorId = $floorId;
  }
  /**
   * @return string
   */
  public function getFloorId()
  {
    return $this->floorId;
  }
  /**
   * @param string
   */
  public function setFloorSectionId($floorSectionId)
  {
    $this->floorSectionId = $floorSectionId;
  }
  /**
   * @return string
   */
  public function getFloorSectionId()
  {
    return $this->floorSectionId;
  }
  /**
   * @param string
   */
  public function setLabel($label)
  {
    $this->label = $label;
  }
  /**
   * @return string
   */
  public function getLabel()
  {
    return $this->label;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EventWorkingLocationPropertiesOfficeLocation::class, 'Google_Service_Calendar_EventWorkingLocationPropertiesOfficeLocation');
