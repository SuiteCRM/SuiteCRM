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

class GoogleCloudAiplatformV1GenerateContentResponsePromptFeedback extends \Google\Collection
{
  protected $collection_key = 'safetyRatings';
  /**
   * @var string
   */
  public $blockReason;
  /**
   * @var string
   */
  public $blockReasonMessage;
  protected $safetyRatingsType = GoogleCloudAiplatformV1SafetyRating::class;
  protected $safetyRatingsDataType = 'array';

  /**
   * @param string
   */
  public function setBlockReason($blockReason)
  {
    $this->blockReason = $blockReason;
  }
  /**
   * @return string
   */
  public function getBlockReason()
  {
    return $this->blockReason;
  }
  /**
   * @param string
   */
  public function setBlockReasonMessage($blockReasonMessage)
  {
    $this->blockReasonMessage = $blockReasonMessage;
  }
  /**
   * @return string
   */
  public function getBlockReasonMessage()
  {
    return $this->blockReasonMessage;
  }
  /**
   * @param GoogleCloudAiplatformV1SafetyRating[]
   */
  public function setSafetyRatings($safetyRatings)
  {
    $this->safetyRatings = $safetyRatings;
  }
  /**
   * @return GoogleCloudAiplatformV1SafetyRating[]
   */
  public function getSafetyRatings()
  {
    return $this->safetyRatings;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1GenerateContentResponsePromptFeedback::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1GenerateContentResponsePromptFeedback');
