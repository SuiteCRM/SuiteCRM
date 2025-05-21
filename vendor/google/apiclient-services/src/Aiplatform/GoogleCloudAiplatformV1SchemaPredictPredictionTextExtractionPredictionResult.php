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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1SchemaPredictPredictionTextExtractionPredictionResult extends \Google\Collection
{
  protected $collection_key = 'textSegmentStartOffsets';
  /**
   * @var float[]
   */
  public $confidences;
  /**
   * @var string[]
   */
  public $displayNames;
  /**
   * @var string[]
   */
  public $ids;
  /**
   * @var string[]
   */
  public $textSegmentEndOffsets;
  /**
   * @var string[]
   */
  public $textSegmentStartOffsets;

  /**
   * @param float[]
   */
  public function setConfidences($confidences)
  {
    $this->confidences = $confidences;
  }
  /**
   * @return float[]
   */
  public function getConfidences()
  {
    return $this->confidences;
  }
  /**
   * @param string[]
   */
  public function setDisplayNames($displayNames)
  {
    $this->displayNames = $displayNames;
  }
  /**
   * @return string[]
   */
  public function getDisplayNames()
  {
    return $this->displayNames;
  }
  /**
   * @param string[]
   */
  public function setIds($ids)
  {
    $this->ids = $ids;
  }
  /**
   * @return string[]
   */
  public function getIds()
  {
    return $this->ids;
  }
  /**
   * @param string[]
   */
  public function setTextSegmentEndOffsets($textSegmentEndOffsets)
  {
    $this->textSegmentEndOffsets = $textSegmentEndOffsets;
  }
  /**
   * @return string[]
   */
  public function getTextSegmentEndOffsets()
  {
    return $this->textSegmentEndOffsets;
  }
  /**
   * @param string[]
   */
  public function setTextSegmentStartOffsets($textSegmentStartOffsets)
  {
    $this->textSegmentStartOffsets = $textSegmentStartOffsets;
  }
  /**
   * @return string[]
   */
  public function getTextSegmentStartOffsets()
  {
    return $this->textSegmentStartOffsets;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaPredictPredictionTextExtractionPredictionResult::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaPredictPredictionTextExtractionPredictionResult');
