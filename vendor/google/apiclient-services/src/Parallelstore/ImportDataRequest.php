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

class ImportDataRequest extends \Google\Model
{
  protected $destinationParallelstoreType = DestinationParallelstore::class;
  protected $destinationParallelstoreDataType = '';
  /**
   * @var string
   */
  public $requestId;
  /**
   * @var string
   */
  public $serviceAccount;
  protected $sourceGcsBucketType = SourceGcsBucket::class;
  protected $sourceGcsBucketDataType = '';

  /**
   * @param DestinationParallelstore
   */
  public function setDestinationParallelstore(DestinationParallelstore $destinationParallelstore)
  {
    $this->destinationParallelstore = $destinationParallelstore;
  }
  /**
   * @return DestinationParallelstore
   */
  public function getDestinationParallelstore()
  {
    return $this->destinationParallelstore;
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
   * @param SourceGcsBucket
   */
  public function setSourceGcsBucket(SourceGcsBucket $sourceGcsBucket)
  {
    $this->sourceGcsBucket = $sourceGcsBucket;
  }
  /**
   * @return SourceGcsBucket
   */
  public function getSourceGcsBucket()
  {
    return $this->sourceGcsBucket;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ImportDataRequest::class, 'Google_Service_Parallelstore_ImportDataRequest');
