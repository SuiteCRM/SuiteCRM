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

namespace Google\Service\APIManagement;

class HttpOperation extends \Google\Collection
{
  protected $collection_key = 'pathParams';
  /**
   * @var string
   */
  public $method;
  /**
   * @var string
   */
  public $path;
  protected $pathParamsType = HttpOperationPathParam::class;
  protected $pathParamsDataType = 'array';
  protected $queryParamsType = HttpOperationQueryParam::class;
  protected $queryParamsDataType = 'map';
  protected $requestType = HttpOperationHttpRequest::class;
  protected $requestDataType = '';
  protected $responseType = HttpOperationHttpResponse::class;
  protected $responseDataType = '';

  /**
   * @param string
   */
  public function setMethod($method)
  {
    $this->method = $method;
  }
  /**
   * @return string
   */
  public function getMethod()
  {
    return $this->method;
  }
  /**
   * @param string
   */
  public function setPath($path)
  {
    $this->path = $path;
  }
  /**
   * @return string
   */
  public function getPath()
  {
    return $this->path;
  }
  /**
   * @param HttpOperationPathParam[]
   */
  public function setPathParams($pathParams)
  {
    $this->pathParams = $pathParams;
  }
  /**
   * @return HttpOperationPathParam[]
   */
  public function getPathParams()
  {
    return $this->pathParams;
  }
  /**
   * @param HttpOperationQueryParam[]
   */
  public function setQueryParams($queryParams)
  {
    $this->queryParams = $queryParams;
  }
  /**
   * @return HttpOperationQueryParam[]
   */
  public function getQueryParams()
  {
    return $this->queryParams;
  }
  /**
   * @param HttpOperationHttpRequest
   */
  public function setRequest(HttpOperationHttpRequest $request)
  {
    $this->request = $request;
  }
  /**
   * @return HttpOperationHttpRequest
   */
  public function getRequest()
  {
    return $this->request;
  }
  /**
   * @param HttpOperationHttpResponse
   */
  public function setResponse(HttpOperationHttpResponse $response)
  {
    $this->response = $response;
  }
  /**
   * @return HttpOperationHttpResponse
   */
  public function getResponse()
  {
    return $this->response;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(HttpOperation::class, 'Google_Service_APIManagement_HttpOperation');
