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

class GoogleMapsPlacesV1AddressDescriptorLandmark extends \Google\Collection
{
  protected $collection_key = 'types';
  protected $displayNameType = GoogleTypeLocalizedText::class;
  protected $displayNameDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $placeId;
  /**
   * @var string
   */
  public $spatialRelationship;
  /**
   * @var float
   */
  public $straightLineDistanceMeters;
  /**
   * @var float
   */
  public $travelDistanceMeters;
  /**
   * @var string[]
   */
  public $types;

  /**
   * @param GoogleTypeLocalizedText
   */
  public function setDisplayName(GoogleTypeLocalizedText $displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return GoogleTypeLocalizedText
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setPlaceId($placeId)
  {
    $this->placeId = $placeId;
  }
  /**
   * @return string
   */
  public function getPlaceId()
  {
    return $this->placeId;
  }
  /**
   * @param string
   */
  public function setSpatialRelationship($spatialRelationship)
  {
    $this->spatialRelationship = $spatialRelationship;
  }
  /**
   * @return string
   */
  public function getSpatialRelationship()
  {
    return $this->spatialRelationship;
  }
  /**
   * @param float
   */
  public function setStraightLineDistanceMeters($straightLineDistanceMeters)
  {
    $this->straightLineDistanceMeters = $straightLineDistanceMeters;
  }
  /**
   * @return float
   */
  public function getStraightLineDistanceMeters()
  {
    return $this->straightLineDistanceMeters;
  }
  /**
   * @param float
   */
  public function setTravelDistanceMeters($travelDistanceMeters)
  {
    $this->travelDistanceMeters = $travelDistanceMeters;
  }
  /**
   * @return float
   */
  public function getTravelDistanceMeters()
  {
    return $this->travelDistanceMeters;
  }
  /**
   * @param string[]
   */
  public function setTypes($types)
  {
    $this->types = $types;
  }
  /**
   * @return string[]
   */
  public function getTypes()
  {
    return $this->types;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1AddressDescriptorLandmark::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1AddressDescriptorLandmark');
