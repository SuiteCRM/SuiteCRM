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

class GoogleMapsPlacesV1Photo extends \Google\Collection
{
  protected $collection_key = 'authorAttributions';
  protected $authorAttributionsType = GoogleMapsPlacesV1AuthorAttribution::class;
  protected $authorAttributionsDataType = 'array';
  /**
   * @var string
   */
  public $flagContentUri;
  /**
   * @var string
   */
  public $googleMapsUri;
  /**
   * @var int
   */
  public $heightPx;
  /**
   * @var string
   */
  public $name;
  /**
   * @var int
   */
  public $widthPx;

  /**
   * @param GoogleMapsPlacesV1AuthorAttribution[]
   */
  public function setAuthorAttributions($authorAttributions)
  {
    $this->authorAttributions = $authorAttributions;
  }
  /**
   * @return GoogleMapsPlacesV1AuthorAttribution[]
   */
  public function getAuthorAttributions()
  {
    return $this->authorAttributions;
  }
  /**
   * @param string
   */
  public function setFlagContentUri($flagContentUri)
  {
    $this->flagContentUri = $flagContentUri;
  }
  /**
   * @return string
   */
  public function getFlagContentUri()
  {
    return $this->flagContentUri;
  }
  /**
   * @param string
   */
  public function setGoogleMapsUri($googleMapsUri)
  {
    $this->googleMapsUri = $googleMapsUri;
  }
  /**
   * @return string
   */
  public function getGoogleMapsUri()
  {
    return $this->googleMapsUri;
  }
  /**
   * @param int
   */
  public function setHeightPx($heightPx)
  {
    $this->heightPx = $heightPx;
  }
  /**
   * @return int
   */
  public function getHeightPx()
  {
    return $this->heightPx;
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
   * @param int
   */
  public function setWidthPx($widthPx)
  {
    $this->widthPx = $widthPx;
  }
  /**
   * @return int
   */
  public function getWidthPx()
  {
    return $this->widthPx;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1Photo::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1Photo');
