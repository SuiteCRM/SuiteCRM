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

class GoogleMapsPlacesV1AddressDescriptorArea extends \Google\Model
{
  /**
   * @var string
   */
  public $containment;
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
   * @param string
   */
  public function setContainment($containment)
  {
    $this->containment = $containment;
  }
  /**
   * @return string
   */
  public function getContainment()
  {
    return $this->containment;
  }
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1AddressDescriptorArea::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1AddressDescriptorArea');
