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

class GoogleMapsPlacesV1AutocompletePlacesResponseSuggestionQueryPrediction extends \Google\Model
{
  protected $structuredFormatType = GoogleMapsPlacesV1AutocompletePlacesResponseSuggestionStructuredFormat::class;
  protected $structuredFormatDataType = '';
  protected $textType = GoogleMapsPlacesV1AutocompletePlacesResponseSuggestionFormattableText::class;
  protected $textDataType = '';

  /**
   * @param GoogleMapsPlacesV1AutocompletePlacesResponseSuggestionStructuredFormat
   */
  public function setStructuredFormat(GoogleMapsPlacesV1AutocompletePlacesResponseSuggestionStructuredFormat $structuredFormat)
  {
    $this->structuredFormat = $structuredFormat;
  }
  /**
   * @return GoogleMapsPlacesV1AutocompletePlacesResponseSuggestionStructuredFormat
   */
  public function getStructuredFormat()
  {
    return $this->structuredFormat;
  }
  /**
   * @param GoogleMapsPlacesV1AutocompletePlacesResponseSuggestionFormattableText
   */
  public function setText(GoogleMapsPlacesV1AutocompletePlacesResponseSuggestionFormattableText $text)
  {
    $this->text = $text;
  }
  /**
   * @return GoogleMapsPlacesV1AutocompletePlacesResponseSuggestionFormattableText
   */
  public function getText()
  {
    return $this->text;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1AutocompletePlacesResponseSuggestionQueryPrediction::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1AutocompletePlacesResponseSuggestionQueryPrediction');
