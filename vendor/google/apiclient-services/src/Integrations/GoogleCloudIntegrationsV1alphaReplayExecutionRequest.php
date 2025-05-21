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

namespace Google\Service\Integrations;

class GoogleCloudIntegrationsV1alphaReplayExecutionRequest extends \Google\Model
{
  protected $modifiedParametersType = GoogleCloudIntegrationsV1alphaValueType::class;
  protected $modifiedParametersDataType = 'map';
  /**
   * @var string
   */
  public $replayMode;
  /**
   * @var string
   */
  public $replayReason;
  /**
   * @var string
   */
  public $updateMask;

  /**
   * @param GoogleCloudIntegrationsV1alphaValueType[]
   */
  public function setModifiedParameters($modifiedParameters)
  {
    $this->modifiedParameters = $modifiedParameters;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaValueType[]
   */
  public function getModifiedParameters()
  {
    return $this->modifiedParameters;
  }
  /**
   * @param string
   */
  public function setReplayMode($replayMode)
  {
    $this->replayMode = $replayMode;
  }
  /**
   * @return string
   */
  public function getReplayMode()
  {
    return $this->replayMode;
  }
  /**
   * @param string
   */
  public function setReplayReason($replayReason)
  {
    $this->replayReason = $replayReason;
  }
  /**
   * @return string
   */
  public function getReplayReason()
  {
    return $this->replayReason;
  }
  /**
   * @param string
   */
  public function setUpdateMask($updateMask)
  {
    $this->updateMask = $updateMask;
  }
  /**
   * @return string
   */
  public function getUpdateMask()
  {
    return $this->updateMask;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudIntegrationsV1alphaReplayExecutionRequest::class, 'Google_Service_Integrations_GoogleCloudIntegrationsV1alphaReplayExecutionRequest');
