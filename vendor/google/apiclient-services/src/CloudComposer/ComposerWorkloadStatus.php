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

namespace Google\Service\CloudComposer;

class ComposerWorkloadStatus extends \Google\Model
{
  /**
   * @var string
   */
  public $detailedStatusMessage;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $statusMessage;

  /**
   * @param string
   */
  public function setDetailedStatusMessage($detailedStatusMessage)
  {
    $this->detailedStatusMessage = $detailedStatusMessage;
  }
  /**
   * @return string
   */
  public function getDetailedStatusMessage()
  {
    return $this->detailedStatusMessage;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setStatusMessage($statusMessage)
  {
    $this->statusMessage = $statusMessage;
  }
  /**
   * @return string
   */
  public function getStatusMessage()
  {
    return $this->statusMessage;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ComposerWorkloadStatus::class, 'Google_Service_CloudComposer_ComposerWorkloadStatus');
