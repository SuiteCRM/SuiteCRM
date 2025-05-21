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

class CustomBiddingAlgorithmRules extends \Google\Model
{
  /**
   * @var bool
   */
  public $active;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $customBiddingAlgorithmId;
  /**
   * @var string
   */
  public $customBiddingAlgorithmRulesId;
  protected $errorType = CustomBiddingAlgorithmRulesError::class;
  protected $errorDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $rulesType = CustomBiddingAlgorithmRulesRef::class;
  protected $rulesDataType = '';
  /**
   * @var string
   */
  public $state;

  /**
   * @param bool
   */
  public function setActive($active)
  {
    $this->active = $active;
  }
  /**
   * @return bool
   */
  public function getActive()
  {
    return $this->active;
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
  public function setCustomBiddingAlgorithmId($customBiddingAlgorithmId)
  {
    $this->customBiddingAlgorithmId = $customBiddingAlgorithmId;
  }
  /**
   * @return string
   */
  public function getCustomBiddingAlgorithmId()
  {
    return $this->customBiddingAlgorithmId;
  }
  /**
   * @param string
   */
  public function setCustomBiddingAlgorithmRulesId($customBiddingAlgorithmRulesId)
  {
    $this->customBiddingAlgorithmRulesId = $customBiddingAlgorithmRulesId;
  }
  /**
   * @return string
   */
  public function getCustomBiddingAlgorithmRulesId()
  {
    return $this->customBiddingAlgorithmRulesId;
  }
  /**
   * @param CustomBiddingAlgorithmRulesError
   */
  public function setError(CustomBiddingAlgorithmRulesError $error)
  {
    $this->error = $error;
  }
  /**
   * @return CustomBiddingAlgorithmRulesError
   */
  public function getError()
  {
    return $this->error;
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
   * @param CustomBiddingAlgorithmRulesRef
   */
  public function setRules(CustomBiddingAlgorithmRulesRef $rules)
  {
    $this->rules = $rules;
  }
  /**
   * @return CustomBiddingAlgorithmRulesRef
   */
  public function getRules()
  {
    return $this->rules;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CustomBiddingAlgorithmRules::class, 'Google_Service_DisplayVideo_CustomBiddingAlgorithmRules');
