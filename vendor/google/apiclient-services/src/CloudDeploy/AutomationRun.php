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

class AutomationRun extends \Google\Model
{
  protected $advanceRolloutOperationType = AdvanceRolloutOperation::class;
  protected $advanceRolloutOperationDataType = '';
  /**
   * @var string
   */
  public $automationId;
  protected $automationSnapshotType = Automation::class;
  protected $automationSnapshotDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string
   */
  public $expireTime;
  /**
   * @var string
   */
  public $name;
  protected $policyViolationType = PolicyViolation::class;
  protected $policyViolationDataType = '';
  protected $promoteReleaseOperationType = PromoteReleaseOperation::class;
  protected $promoteReleaseOperationDataType = '';
  protected $repairRolloutOperationType = RepairRolloutOperation::class;
  protected $repairRolloutOperationDataType = '';
  /**
   * @var string
   */
  public $ruleId;
  /**
   * @var string
   */
  public $serviceAccount;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $stateDescription;
  /**
   * @var string
   */
  public $targetId;
  protected $timedPromoteReleaseOperationType = TimedPromoteReleaseOperation::class;
  protected $timedPromoteReleaseOperationDataType = '';
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string
   */
  public $waitUntilTime;

  /**
   * @param AdvanceRolloutOperation
   */
  public function setAdvanceRolloutOperation(AdvanceRolloutOperation $advanceRolloutOperation)
  {
    $this->advanceRolloutOperation = $advanceRolloutOperation;
  }
  /**
   * @return AdvanceRolloutOperation
   */
  public function getAdvanceRolloutOperation()
  {
    return $this->advanceRolloutOperation;
  }
  /**
   * @param string
   */
  public function setAutomationId($automationId)
  {
    $this->automationId = $automationId;
  }
  /**
   * @return string
   */
  public function getAutomationId()
  {
    return $this->automationId;
  }
  /**
   * @param Automation
   */
  public function setAutomationSnapshot(Automation $automationSnapshot)
  {
    $this->automationSnapshot = $automationSnapshot;
  }
  /**
   * @return Automation
   */
  public function getAutomationSnapshot()
  {
    return $this->automationSnapshot;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setEtag($etag)
  {
    $this->etag = $etag;
  }
  /**
   * @return string
   */
  public function getEtag()
  {
    return $this->etag;
  }
  /**
   * @param string
   */
  public function setExpireTime($expireTime)
  {
    $this->expireTime = $expireTime;
  }
  /**
   * @return string
   */
  public function getExpireTime()
  {
    return $this->expireTime;
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
   * @param PolicyViolation
   */
  public function setPolicyViolation(PolicyViolation $policyViolation)
  {
    $this->policyViolation = $policyViolation;
  }
  /**
   * @return PolicyViolation
   */
  public function getPolicyViolation()
  {
    return $this->policyViolation;
  }
  /**
   * @param PromoteReleaseOperation
   */
  public function setPromoteReleaseOperation(PromoteReleaseOperation $promoteReleaseOperation)
  {
    $this->promoteReleaseOperation = $promoteReleaseOperation;
  }
  /**
   * @return PromoteReleaseOperation
   */
  public function getPromoteReleaseOperation()
  {
    return $this->promoteReleaseOperation;
  }
  /**
   * @param RepairRolloutOperation
   */
  public function setRepairRolloutOperation(RepairRolloutOperation $repairRolloutOperation)
  {
    $this->repairRolloutOperation = $repairRolloutOperation;
  }
  /**
   * @return RepairRolloutOperation
   */
  public function getRepairRolloutOperation()
  {
    return $this->repairRolloutOperation;
  }
  /**
   * @param string
   */
  public function setRuleId($ruleId)
  {
    $this->ruleId = $ruleId;
  }
  /**
   * @return string
   */
  public function getRuleId()
  {
    return $this->ruleId;
  }
  /**
   * @param string
   */
  public function setServiceAccount($serviceAccount)
  {
    $this->serviceAccount = $serviceAccount;
  }
  /**
   * @return string
   */
  public function getServiceAccount()
  {
    return $this->serviceAccount;
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
  public function setStateDescription($stateDescription)
  {
    $this->stateDescription = $stateDescription;
  }
  /**
   * @return string
   */
  public function getStateDescription()
  {
    return $this->stateDescription;
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
   * @param TimedPromoteReleaseOperation
   */
  public function setTimedPromoteReleaseOperation(TimedPromoteReleaseOperation $timedPromoteReleaseOperation)
  {
    $this->timedPromoteReleaseOperation = $timedPromoteReleaseOperation;
  }
  /**
   * @return TimedPromoteReleaseOperation
   */
  public function getTimedPromoteReleaseOperation()
  {
    return $this->timedPromoteReleaseOperation;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
  /**
   * @param string
   */
  public function setWaitUntilTime($waitUntilTime)
  {
    $this->waitUntilTime = $waitUntilTime;
  }
  /**
   * @return string
   */
  public function getWaitUntilTime()
  {
    return $this->waitUntilTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AutomationRun::class, 'Google_Service_CloudDeploy_AutomationRun');
