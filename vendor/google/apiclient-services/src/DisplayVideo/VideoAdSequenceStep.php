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

namespace Google\Service\DisplayVideo;

class VideoAdSequenceStep extends \Google\Model
{
  /**
   * @var string
   */
  public $adGroupId;
  /**
   * @var string
   */
  public $interactionType;
  /**
   * @var string
   */
  public $previousStepId;
  /**
   * @var string
   */
  public $stepId;

  /**
   * @param string
   */
  public function setAdGroupId($adGroupId)
  {
    $this->adGroupId = $adGroupId;
  }
  /**
   * @return string
   */
  public function getAdGroupId()
  {
    return $this->adGroupId;
  }
  /**
   * @param string
   */
  public function setInteractionType($interactionType)
  {
    $this->interactionType = $interactionType;
  }
  /**
   * @return string
   */
  public function getInteractionType()
  {
    return $this->interactionType;
  }
  /**
   * @param string
   */
  public function setPreviousStepId($previousStepId)
  {
    $this->previousStepId = $previousStepId;
  }
  /**
   * @return string
   */
  public function getPreviousStepId()
  {
    return $this->previousStepId;
  }
  /**
   * @param string
   */
  public function setStepId($stepId)
  {
    $this->stepId = $stepId;
  }
  /**
   * @return string
   */
  public function getStepId()
  {
    return $this->stepId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VideoAdSequenceStep::class, 'Google_Service_DisplayVideo_VideoAdSequenceStep');
