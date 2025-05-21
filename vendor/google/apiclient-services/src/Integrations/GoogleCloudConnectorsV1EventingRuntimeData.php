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

class GoogleCloudConnectorsV1EventingRuntimeData extends \Google\Model
{
  /**
   * @var string
   */
  public $eventsListenerEndpoint;
  /**
   * @var string
   */
  public $eventsListenerPscSa;
  protected $statusType = GoogleCloudConnectorsV1EventingStatus::class;
  protected $statusDataType = '';
  protected $webhookDataType = GoogleCloudConnectorsV1EventingRuntimeDataWebhookData::class;
  protected $webhookDataDataType = '';
  protected $webhookSubscriptionsType = GoogleCloudConnectorsV1EventingRuntimeDataWebhookSubscriptions::class;
  protected $webhookSubscriptionsDataType = '';

  /**
   * @param string
   */
  public function setEventsListenerEndpoint($eventsListenerEndpoint)
  {
    $this->eventsListenerEndpoint = $eventsListenerEndpoint;
  }
  /**
   * @return string
   */
  public function getEventsListenerEndpoint()
  {
    return $this->eventsListenerEndpoint;
  }
  /**
   * @param string
   */
  public function setEventsListenerPscSa($eventsListenerPscSa)
  {
    $this->eventsListenerPscSa = $eventsListenerPscSa;
  }
  /**
   * @return string
   */
  public function getEventsListenerPscSa()
  {
    return $this->eventsListenerPscSa;
  }
  /**
   * @param GoogleCloudConnectorsV1EventingStatus
   */
  public function setStatus(GoogleCloudConnectorsV1EventingStatus $status)
  {
    $this->status = $status;
  }
  /**
   * @return GoogleCloudConnectorsV1EventingStatus
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param GoogleCloudConnectorsV1EventingRuntimeDataWebhookData
   */
  public function setWebhookData(GoogleCloudConnectorsV1EventingRuntimeDataWebhookData $webhookData)
  {
    $this->webhookData = $webhookData;
  }
  /**
   * @return GoogleCloudConnectorsV1EventingRuntimeDataWebhookData
   */
  public function getWebhookData()
  {
    return $this->webhookData;
  }
  /**
   * @param GoogleCloudConnectorsV1EventingRuntimeDataWebhookSubscriptions
   */
  public function setWebhookSubscriptions(GoogleCloudConnectorsV1EventingRuntimeDataWebhookSubscriptions $webhookSubscriptions)
  {
    $this->webhookSubscriptions = $webhookSubscriptions;
  }
  /**
   * @return GoogleCloudConnectorsV1EventingRuntimeDataWebhookSubscriptions
   */
  public function getWebhookSubscriptions()
  {
    return $this->webhookSubscriptions;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudConnectorsV1EventingRuntimeData::class, 'Google_Service_Integrations_GoogleCloudConnectorsV1EventingRuntimeData');
