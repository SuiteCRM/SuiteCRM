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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1betaControl extends \Google\Collection
{
  protected $collection_key = 'useCases';
  /**
   * @var string[]
   */
  public $associatedServingConfigIds;
  protected $boostActionType = GoogleCloudDiscoveryengineV1betaControlBoostAction::class;
  protected $boostActionDataType = '';
  protected $conditionsType = GoogleCloudDiscoveryengineV1betaCondition::class;
  protected $conditionsDataType = 'array';
  /**
   * @var string
   */
  public $displayName;
  protected $filterActionType = GoogleCloudDiscoveryengineV1betaControlFilterAction::class;
  protected $filterActionDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $promoteActionType = GoogleCloudDiscoveryengineV1betaControlPromoteAction::class;
  protected $promoteActionDataType = '';
  protected $redirectActionType = GoogleCloudDiscoveryengineV1betaControlRedirectAction::class;
  protected $redirectActionDataType = '';
  /**
   * @var string
   */
  public $solutionType;
  protected $synonymsActionType = GoogleCloudDiscoveryengineV1betaControlSynonymsAction::class;
  protected $synonymsActionDataType = '';
  /**
   * @var string[]
   */
  public $useCases;

  /**
   * @param string[]
   */
  public function setAssociatedServingConfigIds($associatedServingConfigIds)
  {
    $this->associatedServingConfigIds = $associatedServingConfigIds;
  }
  /**
   * @return string[]
   */
  public function getAssociatedServingConfigIds()
  {
    return $this->associatedServingConfigIds;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaControlBoostAction
   */
  public function setBoostAction(GoogleCloudDiscoveryengineV1betaControlBoostAction $boostAction)
  {
    $this->boostAction = $boostAction;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaControlBoostAction
   */
  public function getBoostAction()
  {
    return $this->boostAction;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaCondition[]
   */
  public function setConditions($conditions)
  {
    $this->conditions = $conditions;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaCondition[]
   */
  public function getConditions()
  {
    return $this->conditions;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaControlFilterAction
   */
  public function setFilterAction(GoogleCloudDiscoveryengineV1betaControlFilterAction $filterAction)
  {
    $this->filterAction = $filterAction;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaControlFilterAction
   */
  public function getFilterAction()
  {
    return $this->filterAction;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaControlPromoteAction
   */
  public function setPromoteAction(GoogleCloudDiscoveryengineV1betaControlPromoteAction $promoteAction)
  {
    $this->promoteAction = $promoteAction;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaControlPromoteAction
   */
  public function getPromoteAction()
  {
    return $this->promoteAction;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaControlRedirectAction
   */
  public function setRedirectAction(GoogleCloudDiscoveryengineV1betaControlRedirectAction $redirectAction)
  {
    $this->redirectAction = $redirectAction;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaControlRedirectAction
   */
  public function getRedirectAction()
  {
    return $this->redirectAction;
  }
  /**
   * @param string
   */
  public function setSolutionType($solutionType)
  {
    $this->solutionType = $solutionType;
  }
  /**
   * @return string
   */
  public function getSolutionType()
  {
    return $this->solutionType;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1betaControlSynonymsAction
   */
  public function setSynonymsAction(GoogleCloudDiscoveryengineV1betaControlSynonymsAction $synonymsAction)
  {
    $this->synonymsAction = $synonymsAction;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaControlSynonymsAction
   */
  public function getSynonymsAction()
  {
    return $this->synonymsAction;
  }
  /**
   * @param string[]
   */
  public function setUseCases($useCases)
  {
    $this->useCases = $useCases;
  }
  /**
   * @return string[]
   */
  public function getUseCases()
  {
    return $this->useCases;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1betaControl::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1betaControl');
