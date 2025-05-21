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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1Trigger extends \Google\Model
{
  protected $onDemandType = GoogleCloudDataplexV1TriggerOnDemand::class;
  protected $onDemandDataType = '';
  protected $scheduleType = GoogleCloudDataplexV1TriggerSchedule::class;
  protected $scheduleDataType = '';

  /**
   * @param GoogleCloudDataplexV1TriggerOnDemand
   */
  public function setOnDemand(GoogleCloudDataplexV1TriggerOnDemand $onDemand)
  {
    $this->onDemand = $onDemand;
  }
  /**
   * @return GoogleCloudDataplexV1TriggerOnDemand
   */
  public function getOnDemand()
  {
    return $this->onDemand;
  }
  /**
   * @param GoogleCloudDataplexV1TriggerSchedule
   */
  public function setSchedule(GoogleCloudDataplexV1TriggerSchedule $schedule)
  {
    $this->schedule = $schedule;
  }
  /**
   * @return GoogleCloudDataplexV1TriggerSchedule
   */
  public function getSchedule()
  {
    return $this->schedule;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1Trigger::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1Trigger');
