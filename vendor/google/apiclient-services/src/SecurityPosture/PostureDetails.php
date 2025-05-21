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

class PostureDetails extends \Google\Model
{
  /**
   * @var string
   */
  public $policySet;
  /**
   * @var string
   */
  public $posture;
  /**
   * @var string
   */
  public $postureDeployment;
  /**
   * @var string
   */
  public $postureDeploymentTargetResource;
  /**
   * @var string
   */
  public $postureRevisionId;

  /**
   * @param string
   */
  public function setPolicySet($policySet)
  {
    $this->policySet = $policySet;
  }
  /**
   * @return string
   */
  public function getPolicySet()
  {
    return $this->policySet;
  }
  /**
   * @param string
   */
  public function setPosture($posture)
  {
    $this->posture = $posture;
  }
  /**
   * @return string
   */
  public function getPosture()
  {
    return $this->posture;
  }
  /**
   * @param string
   */
  public function setPostureDeployment($postureDeployment)
  {
    $this->postureDeployment = $postureDeployment;
  }
  /**
   * @return string
   */
  public function getPostureDeployment()
  {
    return $this->postureDeployment;
  }
  /**
   * @param string
   */
  public function setPostureDeploymentTargetResource($postureDeploymentTargetResource)
  {
    $this->postureDeploymentTargetResource = $postureDeploymentTargetResource;
  }
  /**
   * @return string
   */
  public function getPostureDeploymentTargetResource()
  {
    return $this->postureDeploymentTargetResource;
  }
  /**
   * @param string
   */
  public function setPostureRevisionId($postureRevisionId)
  {
    $this->postureRevisionId = $postureRevisionId;
  }
  /**
   * @return string
   */
  public function getPostureRevisionId()
  {
    return $this->postureRevisionId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PostureDetails::class, 'Google_Service_SecurityPosture_PostureDetails');
