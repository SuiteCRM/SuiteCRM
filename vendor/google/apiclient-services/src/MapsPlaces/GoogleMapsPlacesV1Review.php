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

class GoogleMapsPlacesV1Review extends \Google\Model
{
  protected $authorAttributionType = GoogleMapsPlacesV1AuthorAttribution::class;
  protected $authorAttributionDataType = '';
  /**
   * @var string
   */
  public $flagContentUri;
  /**
   * @var string
   */
  public $googleMapsUri;
  /**
   * @var string
   */
  public $name;
  protected $originalTextType = GoogleTypeLocalizedText::class;
  protected $originalTextDataType = '';
  /**
   * @var string
   */
  public $publishTime;
  public $rating;
  /**
   * @var string
   */
  public $relativePublishTimeDescription;
  protected $textType = GoogleTypeLocalizedText::class;
  protected $textDataType = '';

  /**
   * @param GoogleMapsPlacesV1AuthorAttribution
   */
  public function setAuthorAttribution(GoogleMapsPlacesV1AuthorAttribution $authorAttribution)
  {
    $this->authorAttribution = $authorAttribution;
  }
  /**
   * @return GoogleMapsPlacesV1AuthorAttribution
   */
  public function getAuthorAttribution()
  {
    return $this->authorAttribution;
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
   * @param GoogleTypeLocalizedText
   */
  public function setOriginalText(GoogleTypeLocalizedText $originalText)
  {
    $this->originalText = $originalText;
  }
  /**
   * @return GoogleTypeLocalizedText
   */
  public function getOriginalText()
  {
    return $this->originalText;
  }
  /**
   * @param string
   */
  public function setPublishTime($publishTime)
  {
    $this->publishTime = $publishTime;
  }
  /**
   * @return string
   */
  public function getPublishTime()
  {
    return $this->publishTime;
  }
  public function setRating($rating)
  {
    $this->rating = $rating;
  }
  public function getRating()
  {
    return $this->rating;
  }
  /**
   * @param string
   */
  public function setRelativePublishTimeDescription($relativePublishTimeDescription)
  {
    $this->relativePublishTimeDescription = $relativePublishTimeDescription;
  }
  /**
   * @return string
   */
  public function getRelativePublishTimeDescription()
  {
    return $this->relativePublishTimeDescription;
  }
  /**
   * @param GoogleTypeLocalizedText
   */
  public function setText(GoogleTypeLocalizedText $text)
  {
    $this->text = $text;
  }
  /**
   * @return GoogleTypeLocalizedText
   */
  public function getText()
  {
    return $this->text;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1Review::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1Review');
