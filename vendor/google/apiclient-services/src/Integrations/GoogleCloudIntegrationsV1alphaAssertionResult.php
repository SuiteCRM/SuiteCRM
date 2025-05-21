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

class GoogleCloudIntegrationsV1alphaAssertionResult extends \Google\Model
{
  protected $assertionType = GoogleCloudIntegrationsV1alphaAssertion::class;
  protected $assertionDataType = '';
  /**
   * @var string
   */
  public $failureMessage;
  /**
   * @var string
   */
  public $status;
  /**
   * @var string
   */
  public $taskName;
  /**
   * @var string
   */
  public $taskNumber;

  /**
   * @param GoogleCloudIntegrationsV1alphaAssertion
   */
  public function setAssertion(GoogleCloudIntegrationsV1alphaAssertion $assertion)
  {
    $this->assertion = $assertion;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaAssertion
   */
  public function getAssertion()
  {
    return $this->assertion;
  }
  /**
   * @param string
   */
  public function setFailureMessage($failureMessage)
  {
    $this->failureMessage = $failureMessage;
  }
  /**
   * @return string
   */
  public function getFailureMessage()
  {
    return $this->failureMessage;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param string
   */
  public function setTaskName($taskName)
  {
    $this->taskName = $taskName;
  }
  /**
   * @return string
   */
  public function getTaskName()
  {
    return $this->taskName;
  }
  /**
   * @param string
   */
  public function setTaskNumber($taskNumber)
  {
    $this->taskNumber = $taskNumber;
  }
  /**
   * @return string
   */
  public function getTaskNumber()
  {
    return $this->taskNumber;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudIntegrationsV1alphaAssertionResult::class, 'Google_Service_Integrations_GoogleCloudIntegrationsV1alphaAssertionResult');
