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

class GoogleCloudAiplatformV1SchemaPredictInstanceVideoClassificationPredictionInstance extends \Google\Model
{
  /**
   * @var string
   */
  public $content;
  /**
   * @var string
   */
  public $mimeType;
  /**
   * @var string
   */
  public $timeSegmentEnd;
  /**
   * @var string
   */
  public $timeSegmentStart;

  /**
   * @param string
   */
  public function setContent($content)
  {
    $this->content = $content;
  }
  /**
   * @return string
   */
  public function getContent()
  {
    return $this->content;
  }
  /**
   * @param string
   */
  public function setMimeType($mimeType)
  {
    $this->mimeType = $mimeType;
  }
  /**
   * @return string
   */
  public function getMimeType()
  {
    return $this->mimeType;
  }
  /**
   * @param string
   */
  public function setTimeSegmentEnd($timeSegmentEnd)
  {
    $this->timeSegmentEnd = $timeSegmentEnd;
  }
  /**
   * @return string
   */
  public function getTimeSegmentEnd()
  {
    return $this->timeSegmentEnd;
  }
  /**
   * @param string
   */
  public function setTimeSegmentStart($timeSegmentStart)
  {
    $this->timeSegmentStart = $timeSegmentStart;
  }
  /**
   * @return string
   */
  public function getTimeSegmentStart()
  {
    return $this->timeSegmentStart;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaPredictInstanceVideoClassificationPredictionInstance::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaPredictInstanceVideoClassificationPredictionInstance');
