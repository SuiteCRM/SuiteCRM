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

namespace Google\Service\Parallelstore;

class ExportDataRequest extends \Google\Model
{
  protected $destinationGcsBucketType = DestinationGcsBucket::class;
  protected $destinationGcsBucketDataType = '';
  /**
   * @var string
   */
  public $requestId;
  /**
   * @var string
   */
  public $serviceAccount;
  protected $sourceParallelstoreType = SourceParallelstore::class;
  protected $sourceParallelstoreDataType = '';

  /**
   * @param DestinationGcsBucket
   */
  public function setDestinationGcsBucket(DestinationGcsBucket $destinationGcsBucket)
  {
    $this->destinationGcsBucket = $destinationGcsBucket;
  }
  /**
   * @return DestinationGcsBucket
   */
  public function getDestinationGcsBucket()
  {
    return $this->destinationGcsBucket;
  }
  /**
   * @param string
   */
  public function setRequestId($requestId)
  {
    $this->requestId = $requestId;
  }
  /**
   * @return string
   */
  public function getRequestId()
  {
    return $this->requestId;
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
   * @param SourceParallelstore
   */
  public function setSourceParallelstore(SourceParallelstore $sourceParallelstore)
  {
    $this->sourceParallelstore = $sourceParallelstore;
  }
  /**
   * @return SourceParallelstore
   */
  public function getSourceParallelstore()
  {
    return $this->sourceParallelstore;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ExportDataRequest::class, 'Google_Service_Parallelstore_ExportDataRequest');
