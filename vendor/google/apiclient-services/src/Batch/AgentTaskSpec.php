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

namespace Google\Service\Batch;

class AgentTaskSpec extends \Google\Collection
{
  protected $collection_key = 'runnables';
  protected $environmentType = AgentEnvironment::class;
  protected $environmentDataType = '';
  protected $loggingOptionType = AgentTaskLoggingOption::class;
  protected $loggingOptionDataType = '';
  /**
   * @var string
   */
  public $maxRunDuration;
  protected $runnablesType = AgentTaskRunnable::class;
  protected $runnablesDataType = 'array';
  protected $userAccountType = AgentTaskUserAccount::class;
  protected $userAccountDataType = '';

  /**
   * @param AgentEnvironment
   */
  public function setEnvironment(AgentEnvironment $environment)
  {
    $this->environment = $environment;
  }
  /**
   * @return AgentEnvironment
   */
  public function getEnvironment()
  {
    return $this->environment;
  }
  /**
   * @param AgentTaskLoggingOption
   */
  public function setLoggingOption(AgentTaskLoggingOption $loggingOption)
  {
    $this->loggingOption = $loggingOption;
  }
  /**
   * @return AgentTaskLoggingOption
   */
  public function getLoggingOption()
  {
    return $this->loggingOption;
  }
  /**
   * @param string
   */
  public function setMaxRunDuration($maxRunDuration)
  {
    $this->maxRunDuration = $maxRunDuration;
  }
  /**
   * @return string
   */
  public function getMaxRunDuration()
  {
    return $this->maxRunDuration;
  }
  /**
   * @param AgentTaskRunnable[]
   */
  public function setRunnables($runnables)
  {
    $this->runnables = $runnables;
  }
  /**
   * @return AgentTaskRunnable[]
   */
  public function getRunnables()
  {
    return $this->runnables;
  }
  /**
   * @param AgentTaskUserAccount
   */
  public function setUserAccount(AgentTaskUserAccount $userAccount)
  {
    $this->userAccount = $userAccount;
  }
  /**
   * @return AgentTaskUserAccount
   */
  public function getUserAccount()
  {
    return $this->userAccount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AgentTaskSpec::class, 'Google_Service_Batch_AgentTaskSpec');
