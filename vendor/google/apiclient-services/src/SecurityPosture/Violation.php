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

namespace Google\Service\SecurityPosture;

class Violation extends \Google\Model
{
  /**
   * @var string
   */
  public $assetId;
  /**
   * @var string
   */
  public $nextSteps;
  /**
   * @var string
   */
  public $policyId;
  /**
   * @var string
   */
  public $severity;
  protected $violatedAssetType = AssetDetails::class;
  protected $violatedAssetDataType = '';
  protected $violatedPolicyType = PolicyDetails::class;
  protected $violatedPolicyDataType = '';
  protected $violatedPostureType = PostureDetails::class;
  protected $violatedPostureDataType = '';

  /**
   * @param string
   */
  public function setAssetId($assetId)
  {
    $this->assetId = $assetId;
  }
  /**
   * @return string
   */
  public function getAssetId()
  {
    return $this->assetId;
  }
  /**
   * @param string
   */
  public function setNextSteps($nextSteps)
  {
    $this->nextSteps = $nextSteps;
  }
  /**
   * @return string
   */
  public function getNextSteps()
  {
    return $this->nextSteps;
  }
  /**
   * @param string
   */
  public function setPolicyId($policyId)
  {
    $this->policyId = $policyId;
  }
  /**
   * @return string
   */
  public function getPolicyId()
  {
    return $this->policyId;
  }
  /**
   * @param string
   */
  public function setSeverity($severity)
  {
    $this->severity = $severity;
  }
  /**
   * @return string
   */
  public function getSeverity()
  {
    return $this->severity;
  }
  /**
   * @param AssetDetails
   */
  public function setViolatedAsset(AssetDetails $violatedAsset)
  {
    $this->violatedAsset = $violatedAsset;
  }
  /**
   * @return AssetDetails
   */
  public function getViolatedAsset()
  {
    return $this->violatedAsset;
  }
  /**
   * @param PolicyDetails
   */
  public function setViolatedPolicy(PolicyDetails $violatedPolicy)
  {
    $this->violatedPolicy = $violatedPolicy;
  }
  /**
   * @return PolicyDetails
   */
  public function getViolatedPolicy()
  {
    return $this->violatedPolicy;
  }
  /**
   * @param PostureDetails
   */
  public function setViolatedPosture(PostureDetails $violatedPosture)
  {
    $this->violatedPosture = $violatedPosture;
  }
  /**
   * @return PostureDetails
   */
  public function getViolatedPosture()
  {
    return $this->violatedPosture;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Violation::class, 'Google_Service_SecurityPosture_Violation');
