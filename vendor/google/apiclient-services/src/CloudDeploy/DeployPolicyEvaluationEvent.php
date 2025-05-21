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

class DeployPolicyEvaluationEvent extends \Google\Collection
{
  protected $collection_key = 'overrides';
  /**
   * @var bool
   */
  public $allowed;
  /**
   * @var string
   */
  public $deliveryPipeline;
  /**
   * @var string
   */
  public $deployPolicy;
  /**
   * @var string
   */
  public $deployPolicyUid;
  /**
   * @var string
   */
  public $invoker;
  /**
   * @var string
   */
  public $message;
  /**
   * @var string[]
   */
  public $overrides;
  /**
   * @var string
   */
  public $pipelineUid;
  /**
   * @var string
   */
  public $rule;
  /**
   * @var string
   */
  public $ruleType;
  /**
   * @var string
   */
  public $target;
  /**
   * @var string
   */
  public $targetUid;
  /**
   * @var string
   */
  public $verdict;

  /**
   * @param bool
   */
  public function setAllowed($allowed)
  {
    $this->allowed = $allowed;
  }
  /**
   * @return bool
   */
  public function getAllowed()
  {
    return $this->allowed;
  }
  /**
   * @param string
   */
  public function setDeliveryPipeline($deliveryPipeline)
  {
    $this->deliveryPipeline = $deliveryPipeline;
  }
  /**
   * @return string
   */
  public function getDeliveryPipeline()
  {
    return $this->deliveryPipeline;
  }
  /**
   * @param string
   */
  public function setDeployPolicy($deployPolicy)
  {
    $this->deployPolicy = $deployPolicy;
  }
  /**
   * @return string
   */
  public function getDeployPolicy()
  {
    return $this->deployPolicy;
  }
  /**
   * @param string
   */
  public function setDeployPolicyUid($deployPolicyUid)
  {
    $this->deployPolicyUid = $deployPolicyUid;
  }
  /**
   * @return string
   */
  public function getDeployPolicyUid()
  {
    return $this->deployPolicyUid;
  }
  /**
   * @param string
   */
  public function setInvoker($invoker)
  {
    $this->invoker = $invoker;
  }
  /**
   * @return string
   */
  public function getInvoker()
  {
    return $this->invoker;
  }
  /**
   * @param string
   */
  public function setMessage($message)
  {
    $this->message = $message;
  }
  /**
   * @return string
   */
  public function getMessage()
  {
    return $this->message;
  }
  /**
   * @param string[]
   */
  public function setOverrides($overrides)
  {
    $this->overrides = $overrides;
  }
  /**
   * @return string[]
   */
  public function getOverrides()
  {
    return $this->overrides;
  }
  /**
   * @param string
   */
  public function setPipelineUid($pipelineUid)
  {
    $this->pipelineUid = $pipelineUid;
  }
  /**
   * @return string
   */
  public function getPipelineUid()
  {
    return $this->pipelineUid;
  }
  /**
   * @param string
   */
  public function setRule($rule)
  {
    $this->rule = $rule;
  }
  /**
   * @return string
   */
  public function getRule()
  {
    return $this->rule;
  }
  /**
   * @param string
   */
  public function setRuleType($ruleType)
  {
    $this->ruleType = $ruleType;
  }
  /**
   * @return string
   */
  public function getRuleType()
  {
    return $this->ruleType;
  }
  /**
   * @param string
   */
  public function setTarget($target)
  {
    $this->target = $target;
  }
  /**
   * @return string
   */
  public function getTarget()
  {
    return $this->target;
  }
  /**
   * @param string
   */
  public function setTargetUid($targetUid)
  {
    $this->targetUid = $targetUid;
  }
  /**
   * @return string
   */
  public function getTargetUid()
  {
    return $this->targetUid;
  }
  /**
   * @param string
   */
  public function setVerdict($verdict)
  {
    $this->verdict = $verdict;
  }
  /**
   * @return string
   */
  public function getVerdict()
  {
    return $this->verdict;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DeployPolicyEvaluationEvent::class, 'Google_Service_CloudDeploy_DeployPolicyEvaluationEvent');
