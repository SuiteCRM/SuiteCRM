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

namespace Google\Service\GKEHub;

class PolicyControllerPolicyContentState extends \Google\Model
{
  protected $bundleStatesType = PolicyControllerOnClusterState::class;
  protected $bundleStatesDataType = 'map';
  protected $referentialSyncConfigStateType = PolicyControllerOnClusterState::class;
  protected $referentialSyncConfigStateDataType = '';
  protected $templateLibraryStateType = PolicyControllerOnClusterState::class;
  protected $templateLibraryStateDataType = '';

  /**
   * @param PolicyControllerOnClusterState[]
   */
  public function setBundleStates($bundleStates)
  {
    $this->bundleStates = $bundleStates;
  }
  /**
   * @return PolicyControllerOnClusterState[]
   */
  public function getBundleStates()
  {
    return $this->bundleStates;
  }
  /**
   * @param PolicyControllerOnClusterState
   */
  public function setReferentialSyncConfigState(PolicyControllerOnClusterState $referentialSyncConfigState)
  {
    $this->referentialSyncConfigState = $referentialSyncConfigState;
  }
  /**
   * @return PolicyControllerOnClusterState
   */
  public function getReferentialSyncConfigState()
  {
    return $this->referentialSyncConfigState;
  }
  /**
   * @param PolicyControllerOnClusterState
   */
  public function setTemplateLibraryState(PolicyControllerOnClusterState $templateLibraryState)
  {
    $this->templateLibraryState = $templateLibraryState;
  }
  /**
   * @return PolicyControllerOnClusterState
   */
  public function getTemplateLibraryState()
  {
    return $this->templateLibraryState;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PolicyControllerPolicyContentState::class, 'Google_Service_GKEHub_PolicyControllerPolicyContentState');
