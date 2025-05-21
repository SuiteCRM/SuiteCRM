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

namespace Google\Service\CloudDeploy;

class RollbackTargetRequest extends \Google\Collection
{
  protected $collection_key = 'overrideDeployPolicy';
  /**
   * @var string[]
   */
  public $overrideDeployPolicy;
  /**
   * @var string
   */
  public $releaseId;
  protected $rollbackConfigType = RollbackTargetConfig::class;
  protected $rollbackConfigDataType = '';
  /**
   * @var string
   */
  public $rolloutId;
  /**
   * @var string
   */
  public $rolloutToRollBack;
  /**
   * @var string
   */
  public $targetId;
  /**
   * @var bool
   */
  public $validateOnly;

  /**
   * @param string[]
   */
  public function setOverrideDeployPolicy($overrideDeployPolicy)
  {
    $this->overrideDeployPolicy = $overrideDeployPolicy;
  }
  /**
   * @return string[]
   */
  public function getOverrideDeployPolicy()
  {
    return $this->overrideDeployPolicy;
  }
  /**
   * @param string
   */
  public function setReleaseId($releaseId)
  {
    $this->releaseId = $releaseId;
  }
  /**
   * @return string
   */
  public function getReleaseId()
  {
    return $this->releaseId;
  }
  /**
   * @param RollbackTargetConfig
   */
  public function setRollbackConfig(RollbackTargetConfig $rollbackConfig)
  {
    $this->rollbackConfig = $rollbackConfig;
  }
  /**
   * @return RollbackTargetConfig
   */
  public function getRollbackConfig()
  {
    return $this->rollbackConfig;
  }
  /**
   * @param string
   */
  public function setRolloutId($rolloutId)
  {
    $this->rolloutId = $rolloutId;
  }
  /**
   * @return string
   */
  public function getRolloutId()
  {
    return $this->rolloutId;
  }
  /**
   * @param string
   */
  public function setRolloutToRollBack($rolloutToRollBack)
  {
    $this->rolloutToRollBack = $rolloutToRollBack;
  }
  /**
   * @return string
   */
  public function getRolloutToRollBack()
  {
    return $this->rolloutToRollBack;
  }
  /**
   * @param string
   */
  public function setTargetId($targetId)
  {
    $this->targetId = $targetId;
  }
  /**
   * @return string
   */
  public function getTargetId()
  {
    return $this->targetId;
  }
  /**
   * @param bool
   */
  public function setValidateOnly($validateOnly)
  {
    $this->validateOnly = $validateOnly;
  }
  /**
   * @return bool
   */
  public function getValidateOnly()
  {
    return $this->validateOnly;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RollbackTargetRequest::class, 'Google_Service_CloudDeploy_RollbackTargetRequest');
