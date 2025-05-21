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

class GoogleCloudDiscoveryengineLoggingErrorLog extends \Google\Model
{
  protected $contextType = GoogleCloudDiscoveryengineLoggingErrorContext::class;
  protected $contextDataType = '';
  protected $importPayloadType = GoogleCloudDiscoveryengineLoggingImportErrorContext::class;
  protected $importPayloadDataType = '';
  /**
   * @var string
   */
  public $message;
  /**
   * @var array[]
   */
  public $requestPayload;
  /**
   * @var array[]
   */
  public $responsePayload;
  protected $serviceContextType = GoogleCloudDiscoveryengineLoggingServiceContext::class;
  protected $serviceContextDataType = '';
  protected $statusType = GoogleRpcStatus::class;
  protected $statusDataType = '';

  /**
   * @param GoogleCloudDiscoveryengineLoggingErrorContext
   */
  public function setContext(GoogleCloudDiscoveryengineLoggingErrorContext $context)
  {
    $this->context = $context;
  }
  /**
   * @return GoogleCloudDiscoveryengineLoggingErrorContext
   */
  public function getContext()
  {
    return $this->context;
  }
  /**
   * @param GoogleCloudDiscoveryengineLoggingImportErrorContext
   */
  public function setImportPayload(GoogleCloudDiscoveryengineLoggingImportErrorContext $importPayload)
  {
    $this->importPayload = $importPayload;
  }
  /**
   * @return GoogleCloudDiscoveryengineLoggingImportErrorContext
   */
  public function getImportPayload()
  {
    return $this->importPayload;
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
   * @param array[]
   */
  public function setRequestPayload($requestPayload)
  {
    $this->requestPayload = $requestPayload;
  }
  /**
   * @return array[]
   */
  public function getRequestPayload()
  {
    return $this->requestPayload;
  }
  /**
   * @param array[]
   */
  public function setResponsePayload($responsePayload)
  {
    $this->responsePayload = $responsePayload;
  }
  /**
   * @return array[]
   */
  public function getResponsePayload()
  {
    return $this->responsePayload;
  }
  /**
   * @param GoogleCloudDiscoveryengineLoggingServiceContext
   */
  public function setServiceContext(GoogleCloudDiscoveryengineLoggingServiceContext $serviceContext)
  {
    $this->serviceContext = $serviceContext;
  }
  /**
   * @return GoogleCloudDiscoveryengineLoggingServiceContext
   */
  public function getServiceContext()
  {
    return $this->serviceContext;
  }
  /**
   * @param GoogleRpcStatus
   */
  public function setStatus(GoogleRpcStatus $status)
  {
    $this->status = $status;
  }
  /**
   * @return GoogleRpcStatus
   */
  public function getStatus()
  {
    return $this->status;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineLoggingErrorLog::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineLoggingErrorLog');
